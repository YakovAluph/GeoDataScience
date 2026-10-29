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
