# Load libraries
library(readxl)   # Excel: .xls and .xlsx
library(terra)    # NEW: ASCII grids and TIFF/GeoTIFF rasters
library(sf)       # NEW: GeoJSON points and coordinate transformations
library(xml2)     # NEW: XML records
library(shiny)
library(shinythemes)
library(shinyjs)
library(gstat)
library(sp)
library(ggplot2)
library(leaflet)
library(raster)
library(data.table)
library(dplyr)
library(DT)

# Global options
options(shiny.maxRequestSize = 30*1024^2)  # Increase max file upload size

# Helper function to clean and prepare spatial data
cleanAndPrepareData <- function(data, xcol, ycol, valcol) {
  data <- data %>%
    select(x = !!sym(xcol), y = !!sym(ycol), value = !!sym(valcol)) %>%
    mutate(across(.cols = everything(), as.numeric)) %>%
    filter(!is.na(x) & !is.na(y) & !is.na(value)) %>%
    filter(between(x, -180, 180) & between(y, -90, 90))
  
  if (nrow(data) < 3) stop("Not enough valid spatial data points to proceed.")
  
  coordinates(data) <- ~x + y
  if (!inherits(data, "Spatial")) stop("Data must be spatial before assigning CRS.")
  
  proj4string(data) <- CRS("+proj=longlat +datum=WGS84")
  return(data)
}

# --- Upload formats and readers (used by server.R) --- #
uploadExtensions <- c(
  ".csv", ".txt", ".xls", ".xlsx", ".asc", ".ascii",
  ".tif", ".tiff", ".xml", ".geojson", ".json"
)

# Convert spatial coordinates to the longitude/latitude system used by the app.
# Keep the source measurements; transform cell centres rather than resampling.
uploadPointsToTable <- function(points) {
  if (!nrow(points) || any(sf::st_is_empty(points))) {
    stop("The spatial file must contain non-empty point features.")
  }
  if (!all(as.character(sf::st_geometry_type(points)) == "POINT")) {
    stop("Upload point GeoJSON; polygons, lines and multipoints are not supported.")
  }
  if (is.na(sf::st_crs(points))) stop("The spatial file has no known CRS.")
  points <- sf::st_transform(points, 4326)
  xy <- sf::st_coordinates(points)[, 1:2, drop = FALSE]
  attributes <- sf::st_drop_geometry(points)
  if (any(vapply(attributes, is.list, logical(1)))) {
    stop("GeoJSON properties must be scalar fields, not nested objects or arrays.")
  }
  # Reserve x and y for geometry; an existing property named x becomes x.1.
  result <- data.table::as.data.table(cbind(
    data.frame(x = xy[, 1], y = xy[, 2]), attributes
  ))
  data.table::setnames(result, make.names(names(result), unique = TRUE))
  attr(result, "upload_xy") <- c("x", "y")
  result
}

readUploadRaster <- function(path, raster_crs = "") {
  # Do not infer a geographic CRS just because the extent looks like degrees.
  r <- terra::rast(path, guessCRS = FALSE)
  # Compressed TIFFs can expand far beyond the 30 MB upload size limit.
  if (terra::ncell(r) * terra::nlyr(r) > 5e6) {
    stop("Raster exceeds 5 million cell-band values. Crop or reduce it before uploading.")
  }
  source_crs <- sf::st_crs(terra::crs(r))
  if (is.na(source_crs)) {
    if (is.function(raster_crs)) raster_crs <- raster_crs()
    if (!nzchar(trimws(raster_crs))) {
      stop("Raster has no embedded CRS. Enter its CRS below the upload, e.g. EPSG:4326.")
    }
    source_crs <- sf::st_crs(raster_crs)
    if (is.na(source_crs)) stop("The raster CRS is not recognised.")
  }
  names(r) <- paste0("band_", seq_len(terra::nlyr(r)))
  # NA removes cells only when every band is missing; keep partially valid cells.
  cells <- as.data.frame(r, xy = TRUE, na.rm = NA)
  if (!nrow(cells)) stop("Raster contains no usable cell values.")
  points <- sf::st_as_sf(cells, coords = c("x", "y"), crs = source_crs)
  uploadPointsToTable(points)
}

# Supported XML: repeated <row>, <record>, or <point> elements with flat
# child fields or attributes. Arbitrary XML schemas need a specific mapping.
readUploadXML <- function(path) {
  doc <- xml2::read_xml(path, options = c("NOBLANKS", "NONET"))
  records <- xml2::xml_find_all(
    doc, "//*[local-name()='row' or local-name()='record' or local-name()='point']"
  )
  if (!length(records)) {
    stop("XML needs repeated <row>, <record> or <point> elements with X, Y and value fields.")
  }
  rows <- lapply(records, function(record) {
    children <- xml2::xml_children(record)
    if (any(xml2::xml_length(children) > 0L)) {
      stop("XML records must contain flat fields; nested structures need a custom reader.")
    }
    fields <- c(
      xml2::xml_attrs(record),
      stats::setNames(trimws(xml2::xml_text(children)), xml2::xml_name(children))
    )
    if (!length(fields) || anyDuplicated(names(fields))) {
      stop("Each XML record needs uniquely named attributes or child fields.")
    }
    as.list(fields)
  })
  result <- data.table::rbindlist(rows, fill = TRUE, use.names = TRUE)
  result <- data.table::as.data.table(lapply(result, type.convert, as.is = TRUE))
  data.table::setnames(result, make.names(names(result), unique = TRUE))
  result
}

# --- Source coordinate system selectors --- #
# These choices identify coordinates already in the file; they do not move
# coordinates from one projection to another by changing their label.
projectionChoices <- c(
  "Choose the source projection" = "",
  "UTM (choose datum, zone and hemisphere)" = "utm",
  "Ireland: Irish Transverse Mercator (ITM) - EPSG:2157" = "2157",
  "Ireland: TM75 / Irish Grid - EPSG:29903" = "29903",
  "Great Britain: OSGB36 / British National Grid - EPSG:27700" = "27700",
  "France: RGF93 v1 / Lambert-93 - EPSG:2154" = "2154",
  "New Zealand: NZGD2000 / NZTM2000 - EPSG:2193" = "2193",
  "Canada: NAD83 / Statistics Canada Lambert - EPSG:3347" = "3347",
  "Other projection (enter its EPSG code)" = "custom"
)

projectionControls <- function(prefix, allow_geographic = FALSE) {
  id <- function(suffix) paste0(prefix, suffix)
  choices <- projectionChoices
  if (allow_geographic) {
    choices <- append(choices,
      c("Longitude/Latitude: WGS84 - EPSG:4326" = "longlat"), after = 1L)
  }
  shiny::tagList(
    shiny::selectInput(id("Projection"), "Source projection", choices, selected = ""),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'utm'", id("Projection")),
      shiny::selectInput(id("UtmDatum"), "UTM datum",
        choices = c("Choose the datum" = "", "WGS84 (all 60 zones)" = "WGS84",
                    "NAD83 (zones 1-23, North)" = "NAD83",
                    "ETRS89 (zones 28-38, North)" = "ETRS89"), selected = ""),
      shiny::selectInput(id("UtmZone"), "UTM zone",
        choices = c("Choose zone 1-60" = "",
                    stats::setNames(as.character(1:60), paste("Zone", 1:60))), selected = ""),
      shiny::selectInput(id("UtmHemisphere"), "UTM hemisphere",
        choices = c("Choose the hemisphere" = "", "Northern hemisphere" = "N",
                    "Southern hemisphere" = "S"), selected = ""),
      shiny::helpText("Match the datum, zone and hemisphere in your dataset metadata. N/S is the hemisphere, not a latitude-band letter. Use Other projection for other datum versions, such as NAD83(CSRS).")
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] === 'custom'", id("Projection")),
      shiny::textInput(id("EPSG"), "Source EPSG code", value = "",
                       placeholder = "e.g. 2157 or EPSG:2157")
    )
  )
}

resolveProjectionCRS <- function(projection, utm_datum = "", utm_zone = "",
                                 utm_hemisphere = "", custom_epsg = "",
                                 allow_geographic = FALSE) {
  supplied <- function(x) length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  if (!supplied(projection)) stop("Choose the source projection for your coordinates.")
  if (projection == "utm") {
    if (!supplied(utm_datum) || !utm_datum %in% c("WGS84", "NAD83", "ETRS89")) {
      stop("Choose the UTM datum shown in the dataset metadata.")
    }
    if (!supplied(utm_zone) || !as.character(utm_zone) %in% as.character(1:60)) {
      stop("Choose a UTM zone from 1 to 60.")
    }
    if (!supplied(utm_hemisphere) || !utm_hemisphere %in% c("N", "S")) {
      stop("Choose the Northern or Southern UTM hemisphere.")
    }
    zone <- as.integer(utm_zone)
    if (utm_datum == "WGS84") {
      epsg <- if (utm_hemisphere == "N") 32600L + zone else 32700L + zone
    } else if (utm_datum == "NAD83") {
      if (utm_hemisphere != "N" || !zone %in% 1:23) {
        stop("The NAD83 preset supports UTM zones 1-23 in the Northern hemisphere. Check your metadata or use its exact EPSG code.")
      }
      epsg <- 26900L + zone
    } else {
      if (utm_hemisphere != "N" || !zone %in% 28:38) {
        stop("The ETRS89 preset supports UTM zones 28-38 in the Northern hemisphere. Check your metadata or use its exact EPSG code.")
      }
      epsg <- 25800L + zone
    }
  } else if (projection == "custom") {
    if (!supplied(custom_epsg)) stop("Enter the source EPSG code.")
    epsg <- sub("^EPSG\\s*:\\s*", "", trimws(custom_epsg), ignore.case = TRUE)
    if (!grepl("^[0-9]+$", epsg)) stop("Enter an EPSG number, such as 2157 or EPSG:2157.")
  } else if (projection == "longlat" && allow_geographic) {
    epsg <- 4326L
  } else if (projection %in% unname(projectionChoices) && projection != "") {
    epsg <- projection
  } else {
    stop("Choose a supported source projection or enter its EPSG code.")
  }
  crs <- tryCatch(sf::st_crs(paste0("EPSG:", epsg)),
                  error = function(e) stop("The EPSG code is not recognised by the installed PROJ database."))
  if (is.na(crs)) stop("The EPSG code is not recognised by the installed PROJ database.")
  geographic <- isTRUE(crs$IsGeographic)
  projected <- grepl("^PROJCRS\\[", crs$wkt)
  if (!projected && !(allow_geographic && geographic)) {
    stop("Choose a projected EPSG code for Easting/Northing. For longitude/latitude, select that coordinate type.")
  }
  crs
}

projectionCRSFromInputs <- function(input, prefix, allow_geographic = FALSE) {
  # Only read controls used by the selected projection. This prevents hidden
  # UTM inputs from invalidating an ITM or custom-CRS raster import.
  projection <- input[[paste0(prefix, "Projection")]]
  args <- list(projection = projection, allow_geographic = allow_geographic)
  if (identical(projection, "utm")) {
    args$utm_datum <- input[[paste0(prefix, "UtmDatum")]]
    args$utm_zone <- input[[paste0(prefix, "UtmZone")]]
    args$utm_hemisphere <- input[[paste0(prefix, "UtmHemisphere")]]
  } else if (identical(projection, "custom")) {
    args$custom_epsg <- input[[paste0(prefix, "EPSG")]]
  }
  do.call(resolveProjectionCRS, args)
}

resetProjectionInputs <- function(session, prefix) {
  for (suffix in c("Projection", "UtmDatum", "UtmZone", "UtmHemisphere")) {
    shiny::updateSelectInput(session, paste0(prefix, suffix), selected = "")
  }
  shiny::updateTextInput(session, paste0(prefix, "EPSG"), value = "")
}

# Formula builders in the analysis modules need syntactically valid names.
# Store original labels separately so users can still recognise their columns.
normalizeUploadColumnNames <- function(data) {
  data <- data.table::as.data.table(data)
  original_names <- names(data)
  labels <- attr(data, "upload_column_labels")
  if (is.null(labels)) labels <- original_names
  safe_names <- make.names(original_names, unique = TRUE)
  # A bare dot has special meaning on the RHS of a model formula.
  safe_names[safe_names == "."] <- "X."
  safe_names <- make.unique(safe_names)
  data.table::setnames(data, safe_names)
  attr(data, "upload_column_labels") <- stats::setNames(unname(labels), safe_names)
  spatial_xy <- attr(data, "upload_xy")
  if (!is.null(spatial_xy)) {
    attr(data, "upload_xy") <- safe_names[match(spatial_xy, original_names)]
  }
  data
}

uploadColumnLabels <- function(data) {
  labels <- attr(data, "upload_column_labels")
  if (is.null(labels)) names(data) else unname(labels[names(data)])
}

readUploadedData <- function(path, original_name, raster_crs = "") {
  ext <- tolower(tools::file_ext(original_name))
  if (!paste0(".", ext) %in% uploadExtensions) stop("Unsupported file type.")

  # Shiny datapaths have no useful extension. Stage spatial files with one so
  # their drivers identify the format reliably. No user filename becomes a path.
  if (ext %in% c("asc", "ascii", "tif", "tiff", "geojson", "json")) {
    staging <- tempfile("geostats-upload-")
    dir.create(staging)
    on.exit(unlink(staging, recursive = TRUE), add = TRUE)
    staged_ext <- if (ext == "ascii") "asc" else ext
    staged_path <- file.path(staging, paste0("data.", staged_ext))
    if (!file.copy(path, staged_path)) stop("Could not prepare the uploaded file.")
    path <- staged_path
  }

  if (ext %in% c("asc", "ascii")) {
    header <- readLines(path, n = 12L, warn = FALSE)
    is_grid <- any(grepl("^\\s*ncols\\s+", header, ignore.case = TRUE)) &&
      any(grepl("^\\s*nrows\\s+", header, ignore.case = TRUE))
    result <- if (is_grid) readUploadRaster(path, raster_crs) else data.table::fread(path)
  } else {
    result <- switch(ext,
      csv = data.table::fread(path),
      txt = data.table::fread(path),
      xls = data.table::as.data.table(readxl::read_excel(path)),
      xlsx = data.table::as.data.table(readxl::read_excel(path)),
      tif = readUploadRaster(path, raster_crs),
      tiff = readUploadRaster(path, raster_crs),
      xml = readUploadXML(path),
      geojson = uploadPointsToTable(sf::st_read(path, drivers = "GeoJSON", quiet = TRUE)),
      json = uploadPointsToTable(sf::st_read(path, drivers = "GeoJSON", quiet = TRUE))
    )
  }
  if (!nrow(result) || ncol(result) < 3L) {
    stop("The file needs data rows and at least X, Y and one measurement column.")
  }
  normalizeUploadColumnNames(result)
}

# === Bundled geostatistical engine: no separate R/ files required ===
# Shared statistical engine. No Shiny dependencies and no dataset-specific names.
gsWithSeed <- function(seed, expression) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  })
  set.seed(seed)
  force(expression)
}

gsModelNames <- c(Sph = "Spherical", Exp = "Exponential", Gau = "Gaussian",
                  Mat = "Matern", Ste = "Stein Matern", Nug = "Nugget baseline")

gsSpatial <- function(table, crs) {
  methods::as(sf::st_as_sf(as.data.frame(table), coords = c("x", "y"),
                          crs = crs, remove = FALSE), "Spatial")
}

gsPrepare <- function(data, xcol, ycol, valcol, crs, scale = 1,
                      transform = "raw", method = "OK", threshold = 0,
                      secondary = NULL) {
  columns <- c(xcol, ycol, valcol, if (method == "CK") secondary)
  expected_columns <- if (method == "CK") 4L else 3L
  if (length(columns) != expected_columns ||
      any(!nzchar(columns)) || anyDuplicated(columns) || !all(columns %in% names(data))) {
    stop("Choose distinct X, Y, measurement and (for co-kriging) secondary columns.")
  }
  crs <- sf::st_crs(crs)
  if (is.na(crs)) stop("Choose a recognised source CRS.")
  if (!is.finite(scale) || !scale %in% c(1, 1000)) stop("Choose a valid coordinate scale.")
  if (scale != 1 && (isTRUE(crs$IsGeographic) ||
                    !tolower(crs$units_gdal) %in% c("metre", "meter", "metres", "meters", "m"))) {
    stop("Kilometre-to-metre conversion requires a projected CRS measured in metres.")
  }
  xy_names <- attr(data, "upload_xy")
  if (!is.null(xy_names) && identical(c(xcol, ycol), xy_names) &&
      (scale != 1 || !isTRUE(crs == sf::st_crs(4326)))) {
    stop("Generated raster/GeoJSON X/Y are WGS84 longitude/latitude; use that CRS without coordinate scaling.")
  }
  number <- function(x) suppressWarnings(as.numeric(as.character(x)))
  tab <- data.table::data.table(x = number(data[[xcol]]) * scale,
                                y = number(data[[ycol]]) * scale,
                                value = number(data[[valcol]]))
  if (method == "CK") tab$secondary <- number(data[[secondary]])
  finite <- Reduce(`&`, lapply(tab, is.finite))
  dropped <- sum(!finite)
  tab <- tab[finite]
  if (isTRUE(crs$IsGeographic) &&
      any(abs(tab$x) > 180 | abs(tab$y) > 90)) stop("Longitude/latitude coordinates are outside valid bounds.")
  before <- nrow(tab)
  # Average repeated sample locations, rather than discarding observations or
  # jittering exact coordinates. CV folds therefore cannot split duplicates.
  if (method == "CK") {
    tab <- tab[, list(value = mean(value), secondary = mean(secondary), replicates = .N), by = c("x", "y")]
  } else tab <- tab[, list(value = mean(value), replicates = .N), by = c("x", "y")]
  if (nrow(tab) < 10L) stop("At least 10 distinct valid sample locations are needed for model fitting and validation.")
  if (diff(range(tab$x)) == 0 || diff(range(tab$y)) == 0) stop("Samples must span both coordinate axes.")
  if (!transform %in% c("raw", "log1p")) stop("Choose Raw or log(1 + value).")
  if (method == "IK") {
    if (!is.finite(threshold) || threshold <= min(tab$value) || threshold >= max(tab$value))
      stop("The indicator threshold must lie strictly inside the measurement range.")
    if (transform != "raw") stop("Indicator kriging uses the raw measurement threshold; select Raw transformation.")
    tab$model_value <- as.numeric(tab$value > threshold)
  } else {
    if (transform == "log1p" && any(tab$value < 0)) stop("log(1 + value) mode requires non-negative measurements.")
    tab$model_value <- if (transform == "log1p") log1p(tab$value) else tab$value
  }
  if (!is.finite(stats::var(tab$model_value)) || stats::var(tab$model_value) <= 0)
    stop("The selected measurement has no variation after preparation.")
  labels <- stats::setNames(uploadColumnLabels(data), names(data))
  list(points = gsSpatial(tab, crs), table = as.data.frame(tab), crs = crs,
       label = unname(labels[valcol]), transform = transform, method = method,
       threshold = threshold, dropped = dropped, duplicates = before - nrow(tab),
       distance_units = if (isTRUE(crs$IsGeographic)) "km (great-circle distance)" else crs$units_gdal)
}

gsFormula <- function(method) {
  if (method == "UK") model_value ~ x + y else model_value ~ 1
}

gsSampleIndex <- function(n, maximum, seed) {
  if (n <= maximum) seq_len(n) else sort(gsWithSeed(seed, sample.int(n, maximum)))
}

gsEmpirical <- function(points, cfg, seed = cfg$seed) {
  sample <- points[gsSampleIndex(nrow(points), cfg$fit_max, seed), ]
  formula <- gsFormula(cfg$method)
  if (cfg$method == "UK" && qr(stats::model.matrix(formula, sample@data))$rank < 3L)
    stop("Universal-kriging coordinates have a rank-deficient linear trend.")
  initial <- gstat::variogram(formula, sample)
  initial <- initial[is.finite(initial$dist) & is.finite(initial$gamma) & initial$dist > 0, ]
  if (nrow(initial) < 3L) stop("Too few usable lag bins. Check sample geometry or increase the variogram sample limit.")
  cutoff <- max(initial$dist) * cfg$cutoff_fraction
  empirical <- gstat::variogram(formula, sample, cutoff = cutoff, width = cutoff / cfg$bins)
  empirical <- empirical[is.finite(empirical$gamma) & is.finite(empirical$dist) &
                         empirical$dist > 0 & empirical$np > 0, ]
  if (nrow(empirical) < 3L) stop("Too few lag bins at this cutoff; increase the cutoff or reduce lag bins.")
  list(empirical = empirical, sample = sample, cutoff = cutoff, width = cutoff / cfg$bins)
}

gsFitModel <- function(empirical, code) {
  weight <- empirical$np / pmax(empirical$dist, .Machine$double.eps)^2
  sill <- max(stats::weighted.mean(empirical$gamma, weight), .Machine$double.eps)
  if (code == "Nug") {
    fit <- gstat::vgm(psill = sill, model = "Nug", range = 0)
    attr(fit, "SSErr") <- sum(weight * (empirical$gamma - sill)^2)
    attr(fit, "singular") <- FALSE
    return(list(model = fit, warnings = character()))
  }
  if (!code %in% names(gsModelNames)) stop("Unsupported variogram model.")
  nugget <- min(max(empirical$gamma[1] * 0.5, 0), sill * 0.8)
  kappas <- if (code %in% c("Mat", "Ste")) c(0.5, 1.5, 2.5) else 0.5
  fits <- list()
  warnings <- character()
  for (range_fraction in c(0.2, 0.6)) for (kappa in kappas) {
    attempt_warnings <- character()
    fitted <- tryCatch(withCallingHandlers(
      gstat::fit.variogram(empirical,
        gstat::vgm(psill = max(sill - nugget, sill * 0.2), model = code,
                   range = max(empirical$dist) * range_fraction, nugget = nugget, kappa = kappa),
        fit.method = 7, debug.level = 0),
      warning = function(w) {
        attempt_warnings <<- unique(c(attempt_warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }),
      error = function(e) NULL)
    warnings <- unique(c(warnings, attempt_warnings))
    if (!is.null(fitted) && all(is.finite(fitted$psill)) && all(fitted$psill >= 0) &&
        all(is.finite(fitted$range)) && all(fitted$range[fitted$model != "Nug"] > 0) &&
        is.finite(attr(fitted, "SSErr")) && !isTRUE(attr(fitted, "singular")) &&
        !any(grepl("no convergence", attempt_warnings, ignore.case = TRUE))) {
      fits[[length(fits) + 1L]] <- fitted
    }
  }
  if (!length(fits)) stop("No converged, non-singular valid fit for ", code, ". Try another model or adjust lag settings.")
  best <- which.min(vapply(fits, function(f) attr(f, "SSErr"), numeric(1)))
  list(model = fits[[best]], warnings = warnings)
}

gsFitState <- function(points, model, empirical, cfg) {
  fitted <- gsFitModel(empirical$empirical, model)
  state <- list(model = fitted$model, code = model, warnings = fitted$warnings)
  if (cfg$method == "CK") {
    if (!is.finite(stats::var(points$secondary)) || stats::var(points$secondary) <= 0)
      stop("The secondary variable has no variation in a training fold.")
    build <- function(data) {
      g <- gstat::gstat(NULL, id = "primary", formula = model_value ~ 1, data = data, nmax = cfg$nmax)
      gstat::gstat(g, id = "secondary", formula = secondary ~ 1, data = data, nmax = cfg$nmax)
    }
    sample_g <- build(empirical$sample)
    cross <- gstat::variogram(sample_g, cutoff = empirical$cutoff, width = empirical$width)
    # Shared ranges from the primary fit; fit direct/cross sills and enforce
    # positive-semidefinite coregionalisation matrices.
    lmc <- withCallingHandlers(
      gstat::fit.lmc(cross, sample_g, model = state$model, fit.ranges = FALSE,
                     fit.lmc = TRUE, correct.diagonal = 1.01, debug.level = 0),
      warning = function(w) { state$warnings <<- unique(c(state$warnings, conditionMessage(w))); invokeRestart("muffleWarning") })
    full_g <- build(points)
    full_g$model <- lmc$model
    state$gstat <- full_g
    state$model <- lmc$model[["primary"]]
    v <- empirical$empirical
    modelled <- gstat::variogramLine(state$model, dist_vector = v$dist)$gamma
    attr(state$model, "SSErr") <- sum(v$np / v$dist^2 * (v$gamma - modelled)^2)
  }
  state
}

gsPredict <- function(points, newdata, state, cfg) {
  if (cfg$method == "CK") {
    prediction <- stats::predict(state$gstat, newdata, debug.level = 0)
    return(data.frame(model_prediction = prediction$primary.pred, variance = prediction$primary.var))
  }
  args <- list(formula = gsFormula(cfg$method), locations = points, newdata = newdata,
               model = state$model, nmax = cfg$nmax, debug.level = 0)
  if (cfg$method == "SK") args$beta <- mean(points$model_value)
  prediction <- do.call(gstat::krige, args)
  data.frame(model_prediction = prediction$var1.pred, variance = prediction$var1.var)
}

gsInverse <- function(prediction, cfg) {
  if (cfg$method == "IK") pmax(0, pmin(1, prediction))
  else if (cfg$transform == "log1p") expm1(prediction) else prediction
}

gsFolds <- function(points, nfold, type, seed) {
  nfold <- as.integer(nfold)
  if (!is.finite(nfold) || nfold < 2L || nfold > min(10L, nrow(points) - 5L))
    stop("Choose 2-10 validation folds, leaving at least five training locations.")
  if (type == "spatial") {
    xy <- sp::coordinates(points)
    # Standardised coordinate clusters create contiguous geographic holdouts.
    # They are split groups, not a replacement for the source CRS.
    folds <- gsWithSeed(seed, stats::kmeans(scale(xy), centers = nfold, nstart = 10)$cluster)
  } else folds <- gsWithSeed(seed, sample(rep(seq_len(nfold), length.out = nrow(points))))
  if (any(tabulate(folds, nfold) < 1L) || any(nrow(points) - tabulate(folds, nfold) < 5L))
    stop("The fold split leaves too few training samples; reduce the number of folds or use random folds.")
  folds
}

gsCV <- function(points, models, cfg, progress = function(...) NULL) {
  folds <- gsFolds(points, cfg$nfold, cfg$fold_type, cfg$seed)
  evaluate <- gsSampleIndex(nrow(points), cfg$cv_max, cfg$seed + 1L)
  results <- stats::setNames(lapply(models, function(m) list()), models)
  errors <- stats::setNames(rep("", length(models)), models)
  for (fold in sort(unique(folds))) {
    train <- points[folds != fold, ]
    test_index <- evaluate[folds[evaluate] == fold]
    if (!length(test_index)) next
    test <- points[test_index, ]
    empirical <- tryCatch(gsEmpirical(train, cfg, cfg$seed + fold), error = identity)
    for (model in models) {
      progress(paste("Cross-validation fold", fold, "-", gsModelNames[[model]]))
      prediction <- tryCatch({
        if (inherits(empirical, "error")) stop(conditionMessage(empirical))
        state <- gsFitState(train, model, empirical, cfg)
        gsPredict(train, test, state, cfg)
      }, error = identity)
      if (inherits(prediction, "error")) {
        errors[model] <- paste0("Fold ", fold, ": ", conditionMessage(prediction))
        prediction <- data.frame(model_prediction = rep(NA_real_, nrow(test)), variance = NA_real_)
      }
      prediction$prediction <- gsInverse(prediction$model_prediction, cfg)
      prediction$observed <- if (cfg$method == "IK") test$model_value else test$value
      prediction$model_observed <- test$model_value
      prediction$residual <- prediction$observed - prediction$prediction
      prediction$model_residual <- prediction$model_observed - prediction$model_prediction
      prediction$zscore <- ifelse(is.finite(prediction$variance) & prediction$variance > 0,
                                   prediction$model_residual / sqrt(pmax(prediction$variance, 0)), NA_real_)
      prediction$x <- test$x
      prediction$y <- test$y
      prediction$fold <- fold
      prediction$row_id <- test_index
      results[[model]][[length(results[[model]]) + 1L]] <- prediction
    }
  }
  results <- lapply(results, function(parts) do.call(rbind, parts))
  list(results = results, errors = errors, folds = folds, evaluate = evaluate)
}

gsMetrics <- function(cv) {
  good <- is.finite(cv$prediction) & is.finite(cv$observed) &
    is.finite(cv$variance) & cv$variance >= -1e-8
  valid <- cv[good, , drop = FALSE]
  zs <- valid$zscore[is.finite(valid$zscore)]
  data.frame(CV_ME = if (nrow(valid)) mean(valid$residual) else NA_real_,
             CV_RMSE = if (nrow(valid)) sqrt(mean(valid$residual^2)) else NA_real_,
             CV_MAE = if (nrow(valid)) mean(abs(valid$residual)) else NA_real_,
             Z_mean = if (length(zs)) mean(zs) else NA_real_,
             Z_variance = if (length(zs) > 1L) stats::var(zs) else NA_real_,
             CV_valid = nrow(valid), CV_evaluated = nrow(cv),
             CV_coverage = if (nrow(cv)) nrow(valid) / nrow(cv) else 0)
}

gsMetricGeometry <- function(points) {
  geographic <- sf::st_transform(sf::st_as_sf(points), 4326)
  xy <- sf::st_coordinates(geographic)
  # Local metre CRS only for masks and distances, never for variogram fitting.
  local <- paste0("+proj=aeqd +lat_0=", mean(xy[, 2]), " +lon_0=", mean(xy[, 1]),
                   " +datum=WGS84 +units=m +no_defs")
  sf::st_transform(geographic, local)
}

gsGrid <- function(points, cfg) {
  xy <- sp::coordinates(points)
  tab <- expand.grid(x = seq(min(xy[, 1]), max(xy[, 1]), length.out = cfg$resolution),
                     y = seq(min(xy[, 2]), max(xy[, 2]), length.out = cfg$resolution))
  grid <- gsSpatial(tab, sf::st_crs(points@proj4string))
  keep <- rep(TRUE, nrow(grid))
  if (cfg$mask != "none") {
    metric_points <- gsMetricGeometry(points)
    metric_grid <- sf::st_transform(sf::st_as_sf(grid), sf::st_crs(metric_points))
    if (cfg$mask == "hull") {
      hull <- sf::st_convex_hull(sf::st_union(metric_points))
      keep <- lengths(sf::st_intersects(metric_grid, hull)) > 0L
    } else if (cfg$mask == "distance") {
      if (!is.finite(cfg$max_distance) || cfg$max_distance <= 0) stop("Enter a positive mask distance in metres.")
      nearest <- sf::st_nearest_feature(metric_grid, metric_points)
      distances <- sf::st_distance(metric_grid, metric_points[nearest, ], by_element = TRUE)
      keep <- as.numeric(distances) <= cfg$max_distance
    } else stop("Unknown prediction mask.")
  }
  if (!any(keep)) stop("The prediction mask removed every grid cell; increase its distance or resolution.")
  list(grid = grid, table = tab, keep = keep)
}

gsRun <- function(prepared, cfg, progress = function(...) NULL) {
  points <- prepared$points
  cfg$method <- prepared$method
  cfg$transform <- prepared$transform
  cfg$seed <- as.integer(cfg$seed)
  cfg$nmax <- as.integer(cfg$nmax)
  cfg$fit_max <- as.integer(cfg$fit_max)
  cfg$cv_max <- as.integer(cfg$cv_max)
  if (!is.finite(cfg$seed) || cfg$seed < 1L || cfg$seed > 1000000L) stop("Choose a seed from 1 to 1000000.")
  if (!cfg$method %in% c("OK", "SK", "UK", "IK", "CK")) stop("Unsupported kriging method.")
  if (!cfg$selected_model %in% c("Auto", names(gsModelNames))) stop("Choose a variogram model.")
  if (!cfg$criterion %in% c("SSE", "CV_RMSE")) stop("Choose a model-selection criterion.")
  if (!is.finite(cfg$nmax) || cfg$nmax < 5L || cfg$nmax > 500L) stop("Choose 5-500 neighbours.")
  if (!is.finite(cfg$fit_max) || cfg$fit_max < 50L || cfg$fit_max > 5000L) stop("Choose a variogram sample limit of 50-5000.")
  if (!is.finite(cfg$cv_max) || cfg$cv_max < 20L || cfg$cv_max > 5000L) stop("Choose a validation sample limit of 20-5000.")
  if (!is.finite(cfg$resolution) || cfg$resolution < 10L || cfg$resolution > 200L) stop("Choose a grid resolution of 10-200.")
  if (!is.finite(cfg$bins) || cfg$bins < 5L || cfg$bins > 40L ||
      !is.finite(cfg$cutoff_fraction) || cfg$cutoff_fraction < 0.25 || cfg$cutoff_fraction > 1)
    stop("Choose valid lag-bin and cutoff controls.")
  if (cfg$selected_model == "Auto" && cfg$criterion == "CV_RMSE" && !cfg$run_cv)
    stop("Enable cross-validation to select a model by CV RMSE, or choose weighted fit error.")
  models <- if (cfg$selected_model == "Auto") names(gsModelNames) else cfg$selected_model
  progress("Computing the empirical variogram")
  empirical <- gsEmpirical(points, cfg)
  fits <- list()
  rows <- list()
  for (model in models) {
    progress(paste("Fitting", gsModelNames[[model]]))
    fit <- tryCatch(gsFitState(points, model, empirical, cfg), error = identity)
    if (inherits(fit, "error")) {
      rows[[model]] <- data.frame(Model = model, Name = gsModelNames[[model]], Nugget = NA_real_,
        Partial_sill = NA_real_, Range = NA_real_, Kappa = NA_real_, SSE = NA_real_,
        Status = conditionMessage(fit), Fit_warning = "")
    } else {
      fits[[model]] <- fit
      m <- fit$model
      structured <- which(m$model != "Nug")
      rows[[model]] <- data.frame(Model = model, Name = gsModelNames[[model]],
        Nugget = sum(m$psill[m$model == "Nug"]), Partial_sill = sum(m$psill[m$model != "Nug"]),
        Range = if (length(structured)) m$range[structured[1]] else NA_real_,
        Kappa = if (length(structured)) m$kappa[structured[1]] else NA_real_,
        SSE = attr(m, "SSErr"), Status = "Fitted",
        Fit_warning = paste(fit$warnings, collapse = "; "))
    }
  }
  comparison <- do.call(rbind, rows)
  rownames(comparison) <- comparison$Model
  if (!length(fits)) stop("No valid variogram fit. Review lag controls and measurement variation.")
  cv <- NULL
  if (cfg$run_cv) {
    cv <- gsCV(points, names(fits), cfg, progress)
    for (model in rownames(comparison)) {
      metrics <- if (model %in% names(cv$results)) gsMetrics(cv$results[[model]]) else
        data.frame(CV_ME = NA_real_, CV_RMSE = NA_real_, CV_MAE = NA_real_, Z_mean = NA_real_,
                   Z_variance = NA_real_, CV_valid = 0L, CV_evaluated = length(cv$evaluate), CV_coverage = 0)
      for (column in names(metrics)) comparison[model, column] <- metrics[[column]]
      if (model %in% names(cv$errors) && nzchar(cv$errors[model]))
        comparison[model, "Status"] <- paste("CV incomplete:", cv$errors[model])
    }
  }
  eligible <- comparison$Model %in% names(fits) & is.finite(comparison$SSE)
  if (cfg$selected_model == "Auto" && cfg$criterion == "CV_RMSE")
    eligible <- eligible & is.finite(comparison$CV_RMSE) & comparison$CV_coverage == 1
  if (!any(eligible)) stop("No candidate completed validation. Review the model table settings, use fewer folds, or select weighted fit error.")
  metric <- if (cfg$selected_model == "Auto") cfg$criterion else "SSE"
  candidates <- comparison[eligible, , drop = FALSE]
  best <- candidates$Model[which.min(candidates[[metric]])]
  comparison$Selected <- comparison$Model == best
  selected_cv <- if (!is.null(cv)) cv$results[[best]] else NULL
  directional <- tryCatch(gstat::variogram(gsFormula(cfg$method), empirical$sample,
    cutoff = empirical$cutoff, width = empirical$width * 2, alpha = c(0, 45, 90, 135)), error = function(e) NULL)
  result <- list(prepared = prepared, cfg = cfg, model = best, fits = fits, comparison = comparison,
                 empirical = empirical$empirical, cutoff = empirical$cutoff,
                 fit_n = nrow(empirical$sample), directional = directional, cv = selected_cv,
                 folds = if (is.null(cv)) NULL else cv$folds, predictions = NULL, grid = NULL,
                 prediction_label = if (cfg$method == "IK") paste0("P(", prepared$label, " > ", prepared$threshold, ")")
                   else if (cfg$transform == "log1p") paste(prepared$label, "(back-transformed median)") else prepared$label)
  if (!cfg$quick_preview) {
    progress("Building the prediction grid")
    grid <- gsGrid(points, cfg)
    progress("Predicting with the selected model")
    prediction <- gsPredict(points, grid$grid[grid$keep, ], fits[[best]], cfg)
    all <- grid$table
    all$model_prediction <- all$variance <- rep(NA_real_, nrow(all))
    all$model_prediction[grid$keep] <- prediction$model_prediction
    all$variance[grid$keep] <- prediction$variance
    invalid <- !is.finite(all$variance) | all$variance < -1e-8 | !is.finite(all$model_prediction)
    all$model_prediction[invalid] <- all$variance[invalid] <- NA_real_
    all$variance <- pmax(all$variance, 0)
    all$prediction <- gsInverse(all$model_prediction, cfg)
    all$prediction[!is.finite(all$prediction)] <- NA_real_
    all$sd_model <- sqrt(all$variance)
    if (!any(is.finite(all$prediction))) stop("Kriging produced no finite predictions. Try a different model or neighbourhood.")
    result$predictions <- all
    result$grid <- grid$grid
  }
  result
}

gsCountTable <- function(values, max_categories = 20L, numeric_bins = 10L) {
  missing <- is.na(values)
  if (is.numeric(values)) missing <- missing | !is.finite(values)
  values <- values[!missing]
  if (!length(values)) stop("The selected count column has no non-missing values.")
  if (is.numeric(values) && length(unique(values)) > max_categories) {
    breaks <- pretty(range(values), n = numeric_bins)
    values <- cut(values, breaks = breaks, include.lowest = TRUE)
    counts <- table(values)
    mode <- "Numeric values grouped into intervals"
  } else {
    counts <- sort(table(as.character(values)), decreasing = TRUE)
    if (length(counts) > max_categories) {
      remainder_label <- "Other categories (combined)"
      while (remainder_label %in% names(counts)) remainder_label <- paste0(remainder_label, "*")
      counts <- c(counts[seq_len(max_categories)],
        stats::setNames(sum(counts[-seq_len(max_categories)]), remainder_label))
    }
    mode <- "Counts by category/value (top categories plus remainder when needed)"
  }
  list(table = data.frame(Category = names(counts), Count = as.integer(counts)),
       missing = sum(missing), mode = mode)
}

# === Bundled plotting and map helpers ===
gsTheme <- function() ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                 panel.grid.minor = ggplot2::element_blank())

gsDistribution <- function(values, view = "both") {
  values <- suppressWarnings(as.numeric(as.character(values)))
  values <- values[is.finite(values)]
  if (!length(values)) stop("Choose a numeric measurement with finite values.")
  raw <- data.frame(Value = values, Scale = "Raw values")
  if (view == "raw") return(raw)
  if (any(values < 0)) stop("Log distribution views require non-negative measurements; choose Raw values.")
  logged <- data.frame(Value = log1p(values), Scale = "log(1 + value)")
  if (view == "log1p") return(logged)
  both <- rbind(raw, logged)
  both$Scale <- factor(both$Scale, levels = c("Raw values", "log(1 + value)"))
  both
}

gsHistogram <- function(distribution, bins, label) {
  ggplot2::ggplot(distribution, ggplot2::aes(Value)) +
    ggplot2::geom_histogram(bins = bins, fill = "#18536f", colour = "white") +
    ggplot2::facet_wrap(~Scale, scales = "free") +
    ggplot2::labs(title = paste("Histogram:", label), x = "Measurement / transformed measurement", y = "Count") + gsTheme()
}

gsDensity <- function(distribution, label) {
  groups <- split(distribution$Value, distribution$Scale)
  if (any(vapply(groups, function(x) length(unique(x)) < 2L, logical(1))))
    stop("Density estimation needs at least two distinct values in each view.")
  ggplot2::ggplot(distribution, ggplot2::aes(Value)) +
    ggplot2::geom_density(fill = "#4c9fba", alpha = 0.55, colour = "#18536f") +
    ggplot2::facet_wrap(~Scale, scales = "free") +
    ggplot2::labs(title = paste("Density:", label), x = "Measurement / transformed measurement", y = "Density") + gsTheme()
}

gsCountPlot <- function(counts, label) {
  tab <- counts$table
  tab$Category <- factor(tab$Category, levels = rev(unique(tab$Category)))
  ggplot2::ggplot(tab, ggplot2::aes(Category, Count)) +
    ggplot2::geom_col(fill = "#d48639") + ggplot2::coord_flip() +
    ggplot2::labs(title = paste("Count plot:", label), subtitle = paste(counts$mode, "| Missing:", counts$missing),
                   x = NULL, y = "Number of uploaded rows") + gsTheme()
}

gsVariogramPlot <- function(result) {
  curves <- do.call(rbind, lapply(names(result$fits), function(model) {
    line <- gstat::variogramLine(result$fits[[model]]$model, maxdist = result$cutoff, n = 200)
    line$Model <- model
    line
  }))
  selected <- curves[curves$Model == result$model, ]
  ggplot2::ggplot() +
    ggplot2::geom_line(data = curves, ggplot2::aes(dist, gamma, colour = Model), alpha = 0.65) +
    ggplot2::geom_line(data = selected, ggplot2::aes(dist, gamma), colour = "#b43030", linewidth = 1.1) +
    ggplot2::geom_point(data = result$empirical, ggplot2::aes(dist, gamma, size = np), colour = "#18536f") +
    ggplot2::scale_size_area(max_size = 6, name = "Sample pairs") +
    ggplot2::labs(title = paste("Selected:", gsModelNames[[result$model]]),
      subtitle = paste("Variogram sample:", result$fit_n, "locations; red line = selected model"),
      x = paste("Lag distance:", result$prepared$distance_units), y = "Semivariance (model scale)") + gsTheme()
}

gsPairCountPlot <- function(result) {
  ggplot2::ggplot(result$empirical, ggplot2::aes(dist, np)) +
    ggplot2::geom_col(fill = "#18536f") +
    ggplot2::labs(title = "Sample pairs per variogram lag", x = paste("Lag distance:", result$prepared$distance_units),
                   y = "Pair count") + gsTheme()
}

gsDirectionalPlot <- function(result) {
  if (is.null(result$directional) || !nrow(result$directional)) stop("Directional variograms are unavailable for this sample geometry.")
  ggplot2::ggplot(result$directional, ggplot2::aes(dist, gamma, colour = factor(dir.hor), group = dir.hor)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(title = "Directional variogram check", x = paste("Lag distance:", result$prepared$distance_units),
                   y = "Semivariance (model scale)", colour = "Azimuth (degrees)") + gsTheme()
}

gsCVObservedPlot <- function(result) {
  cv <- result$cv
  if (is.null(cv)) stop("Enable cross-validation and run the analysis.")
  cv <- cv[is.finite(cv$prediction) & is.finite(cv$observed), ]
  if (!nrow(cv)) stop("No finite validation predictions are available.")
  metrics <- gsMetrics(result$cv)
  ggplot2::ggplot(cv, ggplot2::aes(prediction, observed)) +
    ggplot2::geom_point(alpha = 0.45, colour = "#18536f") +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#b43030") +
    ggplot2::coord_equal() +
    ggplot2::labs(title = "Cross-validation: observed vs predicted",
      subtitle = sprintf("RMSE %.4g | MAE %.4g | Mean error %.4g | Coverage %.1f%%",
                          metrics$CV_RMSE, metrics$CV_MAE, metrics$CV_ME, 100 * metrics$CV_coverage),
      x = "Held-out prediction", y = if (result$cfg$method == "IK") "Observed indicator (0/1)" else result$prepared$label) + gsTheme()
}

gsCVQQPlot <- function(result) {
  cv <- result$cv
  if (is.null(cv)) stop("Enable cross-validation and run the analysis.")
  cv <- cv[is.finite(cv$zscore), ]
  if (nrow(cv) < 3L) stop("At least three standardised residuals with positive kriging variance are needed.")
  ggplot2::ggplot(cv, ggplot2::aes(sample = zscore)) +
    ggplot2::stat_qq(colour = "#18536f", alpha = 0.55) +
    ggplot2::stat_qq_line(colour = "#b43030") +
    ggplot2::labs(title = "Cross-validation residual Q-Q plot", x = "Normal theoretical quantiles",
                   y = "Standardised residual (model scale)") + gsTheme()
}

gsCVResidualPlot <- function(result) {
  cv <- result$cv
  if (is.null(cv)) stop("Enable cross-validation and run the analysis.")
  cv <- cv[is.finite(cv$residual), ]
  if (!nrow(cv)) stop("No finite residuals are available.")
  ggplot2::ggplot(cv, ggplot2::aes(x, y, colour = residual)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::scale_colour_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0) +
    ggplot2::coord_equal() + ggplot2::labs(title = "Spatial distribution of held-out residuals",
      x = "X in source CRS", y = "Y in source CRS", colour = "Observed - predicted") + gsTheme()
}

gsSpatialScatter <- function(result) {
  ggplot2::ggplot(result$prepared$table, ggplot2::aes(x, y, colour = value)) +
    ggplot2::geom_point(size = 1.6, alpha = 0.75) +
    ggplot2::scale_colour_viridis_c(name = result$prepared$label) +
    ggplot2::coord_equal() + ggplot2::labs(title = "Observed spatial scatter",
      x = "X in source CRS", y = "Y in source CRS") + gsTheme()
}

gsPredictionPlot <- function(result, uncertainty = FALSE) {
  if (is.null(result$predictions)) stop("Disable Quick Preview and run the analysis to predict a grid.")
  tab <- result$predictions
  tab$display <- if (uncertainty) tab$sd_model else tab$prediction
  label <- if (uncertainty) "Kriging SD (model scale)" else result$prediction_label
  ggplot2::ggplot(tab, ggplot2::aes(x, y, fill = display)) +
    ggplot2::geom_raster(na.rm = TRUE) + ggplot2::scale_fill_viridis_c(name = label, na.value = "transparent") +
    ggplot2::coord_equal() + ggplot2::labs(title = if (uncertainty) "Prediction uncertainty" else "Kriging prediction",
      x = "X in source CRS", y = "Y in source CRS") + gsTheme()
}

gsAddHomeButton <- function(map) {
  map <- leaflet::addEasyButton(map, leaflet::easyButton(icon = "fa-home", title = "Reset map view",
    position = "topleft", onClick = htmlwidgets::JS("function(btn, map) {
      var home = map.options.geostatsHomeView;
      if (home) { map.stop(); map.setView(home.center, home.zoom, {animate: false}); }
    }")))
  htmlwidgets::onRender(map, "function(el, x) {
    var map = this;
    map.whenReady(function() {
      var center = map.getCenter();
      map.options.geostatsHomeView = {center: [center.lat, center.lng], zoom: map.getZoom()};
    });
  }")
}
