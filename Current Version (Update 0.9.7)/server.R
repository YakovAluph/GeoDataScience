library(shiny)
library(shinyjs)
library(ggplot2)
library(leaflet)
library(DT)


shinyServer(function(input, output, session) {
  errorMsg <- reactiveVal(NULL)
  analysisResult <- reactiveVal(NULL)
  uploadVersion <- reactiveVal(0L)

  output$fileInputUI <- renderUI({
    uploadVersion()
    fileInput("file", NULL, accept = uploadExtensions)
  })

  output$rasterCRSUI <- renderUI({
    req(input$file)
    if (tolower(tools::file_ext(input$file$name)) %in% c("asc", "ascii", "tif", "tiff")) {
      tagList(h5("Raster source CRS (only if missing from the file)"),
        projectionControls("raster", allow_geographic = TRUE),
        helpText("Embedded raster CRS takes precedence. Imported raster/GeoJSON X/Y are converted to WGS84 longitude/latitude."))
    }
  })

  userData <- reactive({
    req(input$file)
    errorMsg(NULL)
    tryCatch(readUploadedData(input$file$datapath, input$file$name,
      raster_crs = function() projectionCRSFromInputs(input, "raster", allow_geographic = TRUE)$wkt),
      error = function(e) { errorMsg(paste("Upload:", conditionMessage(e))); NULL })
  })

  observeEvent(userData(), {
    analysisResult(NULL)
    if (!is.null(attr(userData(), "upload_xy"))) {
      updateRadioButtons(session, "coordType", selected = "longlat")
      updateSelectInput(session, "coordinateScale", selected = "1")
    }
  })

  selectedInputCRS <- reactive({
    if (identical(input$coordType, "longlat")) return(sf::st_crs(4326))
    if (!identical(input$coordType, "projected")) stop("Choose the coordinate type.")
    imported <- if (is.null(input$file)) NULL else attr(userData(), "upload_xy")
    if (!is.null(imported) && identical(c(input$xcol, input$ycol), imported))
      stop("Generated raster/GeoJSON X/Y are WGS84; select Longitude/Latitude.")
    projectionCRSFromInputs(input, "source")
  })

  output$sourceCRSInfo <- renderUI({
    tryCatch({
      crs <- selectedInputCRS()
      helpText(paste0("Coordinate CRS: ", crs$Name,
        if (!is.na(crs$epsg)) paste0(" (EPSG:", crs$epsg, ")") else ""))
    }, error = function(e) div(style = "color: #a94442;", conditionMessage(e)))
  })

  output$fileRequiredMsg <- renderUI({
    if (is.null(input$file)) div(style = "color: #a94442;", "Upload a dataset to begin.")
  })
  output$dataWarning <- renderUI({
    message <- errorMsg()
    if (!is.null(message)) div(style = "color: #a94442; font-weight: bold;", message)
  })

  output$dataPreview <- DT::renderDT({
    req(userData())
    DT::datatable(userData(), colnames = uploadColumnLabels(userData()), rownames = FALSE,
      escape = TRUE, options = list(pageLength = 25, scrollX = TRUE,
        lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All"))))
  })

  output$columnSelectors <- renderUI({
    req(userData())
    cols <- names(userData())
    choices <- c("Please choose variable" = "", stats::setNames(cols, uploadColumnLabels(userData())))
    xy <- attr(userData(), "upload_xy")
    tagList(
      selectizeInput("xcol", "X Coordinate (Longitude/Easting)", choices,
        selected = if (is.null(xy)) "" else xy[1]),
      selectizeInput("ycol", "Y Coordinate (Latitude/Northing)", choices,
        selected = if (is.null(xy)) "" else xy[2]),
      selectizeInput("valcol", "Measurement", choices),
      conditionalPanel("input.krigingType === 'CK'",
        selectizeInput("secondaryVar", "Secondary variable", choices)))
  })

  selectedLabel <- reactive({
    req(userData(), input$valcol)
    labels <- stats::setNames(uploadColumnLabels(userData()), names(userData()))
    unname(labels[input$valcol])
  })
  observeEvent(list(userData(), input$valcol), {
    req(userData(), input$valcol)
    values <- suppressWarnings(as.numeric(as.character(userData()[[input$valcol]])))
    values <- values[is.finite(values)]
    if (length(values)) updateNumericInput(session, "ikThreshold", value = stats::median(values))
  })

  # Results are snapshots of a Run Analysis click, not reactive recomputations.
  # Changing EDA options or toggling map layers never refits a variogram.
  observeEvent(list(userData(), input$xcol, input$ycol, input$valcol, input$secondaryVar,
    input$coordType, input$sourceProjection, input$sourceUtmDatum, input$sourceUtmZone,
    input$sourceUtmHemisphere, input$sourceEPSG, input$coordinateScale, input$modelType,
    input$krigingType, input$dataTransform, input$ikThreshold, input$gridResolution,
    input$nmax, input$fitMax, input$lagBins, input$cutoffFraction, input$runCV,
    input$cvFolds, input$cvMax, input$cvType, input$cvSeed, input$selectionCriterion,
    input$predictionMask, input$maxMaskDistance, input$quickPreview), {
      analysisResult(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$resetBtn, {
    analysisResult(NULL)
    errorMsg(NULL)
    shinyjs::reset("appForm")
    uploadVersion(uploadVersion() + 1L)
    updateRadioButtons(session, "coordType", selected = "longlat")
    resetProjectionInputs(session, "source")
    resetProjectionInputs(session, "raster")
    updateSelectInput(session, "coordinateScale", selected = "1")
    updateSelectInput(session, "modelType", selected = "Auto")
    updateRadioButtons(session, "krigingType", selected = "OK")
    updateSelectInput(session, "dataTransform", selected = "raw")
  })

  observeEvent(input$plotBtn, {
    errorMsg(NULL)
    analysisResult(NULL)
    req(userData())
    shinyjs::disable("plotBtn")
    on.exit(shinyjs::enable("plotBtn"), add = TRUE)
    tryCatch(withProgress(message = "Preparing geostatistical analysis", value = 0, {
      crs <- selectedInputCRS()
      scale <- if (identical(input$coordType, "longlat")) 1 else as.numeric(input$coordinateScale)
      prepared <- gsPrepare(userData(), input$xcol, input$ycol, input$valcol, crs,
        scale = scale, transform = input$dataTransform, method = input$krigingType,
        threshold = input$ikThreshold, secondary = input$secondaryVar)
      cfg <- list(method = input$krigingType, transform = input$dataTransform,
        selected_model = input$modelType, criterion = input$selectionCriterion,
        nmax = input$nmax, fit_max = input$fitMax, bins = input$lagBins,
        cutoff_fraction = input$cutoffFraction, run_cv = isTRUE(input$runCV),
        nfold = input$cvFolds, cv_max = input$cvMax, fold_type = input$cvType,
        seed = input$cvSeed, resolution = input$gridResolution,
        mask = input$predictionMask, max_distance = input$maxMaskDistance,
        quick_preview = isTRUE(input$quickPreview))
      stages <- 0L
      report <- function(detail) {
        stages <<- stages + 1L
        setProgress(value = min(0.95, 0.1 + 0.85 * stages / (10 + 6 * cfg$nfold)), detail = detail)
      }
      result <- gsRun(prepared, cfg, report)
      analysisResult(result)
      updateTabsetPanel(session, "resultsTabs", selected = "Variogram")
      setProgress(value = 1, detail = "Complete")
    }), error = function(e) errorMsg(paste("Analysis:", conditionMessage(e))))
  })

  resultForPlot <- reactive({
    result <- analysisResult()
    validate(need(!is.null(result), "Choose the data columns and settings, then press Run Analysis."))
    result
  })

  output$analysisStatus <- renderUI({
    result <- analysisResult()
    if (is.null(result)) return(helpText("Exploration plots update with your selected measurement. Press Run Analysis for variograms, validation and kriging."))
    tagList(
      h4(paste(gsModelNames[[result$model]], "|", result$cfg$method)),
      p(sprintf("%s distinct locations; %s invalid rows removed; %s duplicate locations averaged. Variogram uses %s locations.",
        nrow(result$prepared$table), result$prepared$dropped, result$prepared$duplicates, result$fit_n)),
      p(paste("Transformation:", result$cfg$transform, "| Prediction:", result$prediction_label)),
      if (result$model == "Nug") helpText("The selected nugget baseline indicates no spatial continuity supported by this comparison."))
  })

  output$mainTabs <- renderUI({
    req(input$file)
    tabsetPanel(id = "resultsTabs",
      tabPanel("Data exploration",
        selectInput("edaView", "Distribution view", c("Raw values" = "raw", "log(1 + value)" = "log1p", "Raw and log side by side" = "both")),
        sliderInput("histBins", "Histogram bins", min = 5, max = 100, value = 30),
        plotOutput("histogramPlot", height = 350), plotOutput("densityPlot", height = 350),
        uiOutput("countSelector"), plotOutput("countPlot", height = 450),
        downloadButton("downloadHistogram", "Save histogram")),
      tabPanel("Variogram", uiOutput("analysisStatus"),
        plotOutput("variogramPlot", height = 450), DTOutput("modelComparison"),
        helpText("Range parameters differ by model family and are not interchangeable effective ranges. Fitting warnings are retained in the table."),
        plotOutput("pairCountPlot", height = 300), plotOutput("directionalPlot", height = 400),
        downloadButton("downloadModels", "Download model comparison")),
      tabPanel("Cross-validation",
        helpText("The variogram (and co-kriging model, when used) is refitted in every training fold. Held-out locations are removed from both variables. Model selection and diagnostics share folds, so these are not independent test-set scores."),
        DTOutput("cvMetrics"), plotOutput("cvObservedPlot", height = 430),
        plotOutput("cvQQPlot", height = 400), plotOutput("cvResidualPlot", height = 450),
        downloadButton("downloadCV", "Download held-out predictions")),
      tabPanel("Kriging Prediction",
        plotOutput("krigingPlot", height = 480), plotOutput("uncertaintyPlot", height = 400),
        h4("Interactive map"),
        helpText("Use the layer control to show the kriging surface, predicted point scatter, observed samples or uncertainty. Use the home button to restore the initial view."),
        uiOutput("mapNotes"), leafletOutput("krigingMap", height = 600),
        downloadButton("downloadPredictions", "Download prediction grid")),
      tabPanel("Spatial scatter", plotOutput("spatialScatterPlot", height = 550)),
      tabPanel("Voronoi Diagram", plotOutput("voronoiPlot", height = 600)),
      tabPanel("Uploaded Data", DTOutput("dataPreview")))
  })

  distribution <- reactive({
    req(userData(), input$valcol, input$edaView)
    tryCatch(gsDistribution(userData()[[input$valcol]], input$edaView),
      error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$histogramPlot <- renderPlot(gsHistogram(distribution(), input$histBins, selectedLabel()))
  output$densityPlot <- renderPlot({
    tryCatch(gsDensity(distribution(), selectedLabel()), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$countSelector <- renderUI({
    req(userData())
    cols <- names(userData())
    selectInput("countColumn", "Count by column", stats::setNames(cols, uploadColumnLabels(userData())),
      selected = if (length(input$valcol) == 1L && input$valcol %in% cols) input$valcol else cols[1])
  })
  output$countPlot <- renderPlot({
    req(userData(), input$countColumn)
    labels <- stats::setNames(uploadColumnLabels(userData()), names(userData()))
    tryCatch(gsCountPlot(gsCountTable(userData()[[input$countColumn]]), labels[[input$countColumn]]),
      error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  output$variogramPlot <- renderPlot(gsVariogramPlot(resultForPlot()))
  output$pairCountPlot <- renderPlot(gsPairCountPlot(resultForPlot()))
  output$directionalPlot <- renderPlot({
    tryCatch(gsDirectionalPlot(resultForPlot()), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$modelComparison <- DT::renderDT({
    tab <- resultForPlot()$comparison
    for (column in names(tab)) if (is.numeric(tab[[column]])) tab[[column]] <- signif(tab[[column]], 5)
    DT::datatable(tab, rownames = FALSE, escape = TRUE, options = list(scrollX = TRUE, pageLength = 10))
  })
  output$cvMetrics <- DT::renderDT({
    result <- resultForPlot()
    validate(need(!is.null(result$cv), "Enable cross-validation and run the analysis."))
    DT::datatable(gsMetrics(result$cv), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  output$cvObservedPlot <- renderPlot({
    tryCatch(gsCVObservedPlot(resultForPlot()), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$cvQQPlot <- renderPlot({
    tryCatch(gsCVQQPlot(resultForPlot()), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$cvResidualPlot <- renderPlot({
    tryCatch(gsCVResidualPlot(resultForPlot()), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$krigingPlot <- renderPlot({
    tryCatch(gsPredictionPlot(resultForPlot()), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$uncertaintyPlot <- renderPlot({
    tryCatch(gsPredictionPlot(resultForPlot(), TRUE), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })
  output$spatialScatterPlot <- renderPlot(gsSpatialScatter(resultForPlot()))

  mapData <- reactive({
    result <- resultForPlot()
    validate(need(!is.null(result$predictions), "Disable Quick Preview and run the analysis to render the kriging map."))
    grid <- result$predictions
    grid <- grid[is.finite(grid$prediction), ]
    # Limit display-only point layers; the raster and CSV retain every cell.
    indices <- gsSampleIndex(nrow(grid), 10000L, result$cfg$seed)
    pred <- sf::st_transform(sf::st_as_sf(grid[indices, ], coords = c("x", "y"),
      crs = result$prepared$crs, remove = FALSE), 4326)
    observed <- result$prepared$table
    obs_indices <- gsSampleIndex(nrow(observed), 10000L, result$cfg$seed)
    obs <- sf::st_transform(sf::st_as_sf(observed[obs_indices, ], coords = c("x", "y"),
      crs = result$prepared$crs, remove = FALSE), 4326)
    list(predicted = pred, observed = obs, predicted_total = nrow(grid), observed_total = nrow(observed))
  })
  output$mapNotes <- renderUI({
    req(analysisResult())
    result <- analysisResult()
    if (is.null(result$predictions)) return(helpText("Quick Preview has no prediction grid."))
    data <- mapData()
    helpText(sprintf("Point display: %s of %s predicted cells, %s of %s observed locations. Surface and downloads retain the full prediction grid.",
      nrow(data$predicted), data$predicted_total, nrow(data$observed), data$observed_total))
  })
  output$krigingMap <- renderLeaflet({
    result <- resultForPlot()
    data <- mapData()
    tab <- result$predictions
    surface <- raster::rasterFromXYZ(tab[, c("x", "y", "prediction")])
    raster::crs(surface) <- result$prepared$points@proj4string
    sd <- raster::rasterFromXYZ(tab[, c("x", "y", "sd_model")])
    raster::crs(sd) <- result$prepared$points@proj4string
    palette <- leaflet::colorNumeric("viridis", domain = tab$prediction, na.color = "transparent")
    sd_palette <- leaflet::colorNumeric("magma", domain = tab$sd_model, na.color = "transparent")
    obs_palette <- leaflet::colorNumeric("viridis", domain = result$prepared$table$value)
    pxy <- sf::st_coordinates(data$predicted)
    oxy <- sf::st_coordinates(data$observed)
    popup <- paste0("Predicted: ", signif(data$predicted$prediction, 6),
      "<br>Kriging SD (model scale): ", signif(data$predicted$sd_model, 6))
    map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "Roadmap") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellite") %>%
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Topography") %>%
      leaflet::addRasterImage(surface, colors = palette, opacity = 0.7, group = "Kriging surface") %>%
      leaflet::addRasterImage(sd, colors = sd_palette, opacity = 0.7, group = "Kriging SD") %>%
      leaflet::addCircleMarkers(lng = pxy[, 1], lat = pxy[, 2], radius = 3,
        color = palette(data$predicted$prediction), fillOpacity = 0.8, stroke = FALSE,
        popup = popup, group = "Predicted point scatter") %>%
      leaflet::addCircleMarkers(lng = oxy[, 1], lat = oxy[, 2], radius = 3,
        color = obs_palette(data$observed$value), fillOpacity = 0.85, stroke = FALSE,
        popup = paste("Observed:", signif(data$observed$value, 6)), group = "Observed samples") %>%
      leaflet::addLegend(pal = palette, values = tab$prediction,
        title = htmltools::htmlEscape(result$prediction_label), group = "Kriging surface") %>%
      leaflet::addLegend(pal = palette, values = tab$prediction,
        title = "Predicted points", group = "Predicted point scatter") %>%
      leaflet::addLegend(pal = obs_palette, values = result$prepared$table$value,
        title = "Observed samples", group = "Observed samples") %>%
      leaflet::addLegend(pal = sd_palette, values = tab$sd_model,
        title = "Kriging SD (model scale)", group = "Kriging SD") %>%
      leaflet::addLayersControl(baseGroups = c("Roadmap", "Satellite", "Topography"),
        overlayGroups = c("Kriging surface", "Predicted point scatter", "Observed samples", "Kriging SD"),
        options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
      leaflet::hideGroup(c("Predicted point scatter", "Observed samples", "Kriging SD"))
    gsAddHomeButton(map)
  })

  output$voronoiPlot <- renderPlot({
    result <- resultForPlot()
    validate(need(requireNamespace("deldir", quietly = TRUE), "Install the deldir package for the Voronoi diagram."))
    tab <- result$prepared$table
    diagram <- deldir::deldir(tab$x, tab$y, suppressMsge = TRUE)
    tiles <- deldir::tile.list(diagram)
    colours <- leaflet::colorNumeric("viridis", domain = tab$value)(tab$value)
    plot(tiles, fillcol = colours, border = "grey70", main = "Voronoi diagram (source coordinates)")
  })

  output$downloadModels <- downloadHandler(filename = function() "variogram_model_comparison.csv",
    content = function(file) utils::write.csv(resultForPlot()$comparison, file, row.names = FALSE))
  output$downloadCV <- downloadHandler(filename = function() "held_out_predictions.csv", content = function(file) {
    cv <- resultForPlot()$cv
    req(cv)
    utils::write.csv(cv, file, row.names = FALSE)
  })
  output$downloadPredictions <- downloadHandler(filename = function() "kriging_prediction_grid.csv", content = function(file) {
    result <- resultForPlot()
    req(result$predictions)
    export <- result$predictions
    export$source_crs <- result$prepared$crs$input
    export$transform <- result$cfg$transform
    export$method <- result$cfg$method
    utils::write.csv(export, file, row.names = FALSE)
  })
  output$downloadHistogram <- downloadHandler(filename = function() "measurement_histogram.png", content = function(file) {
    ggplot2::ggsave(file, plot = gsHistogram(distribution(), input$histBins, selectedLabel()),
      width = 10, height = 5, dpi = 300, bg = "white")
  })
})
