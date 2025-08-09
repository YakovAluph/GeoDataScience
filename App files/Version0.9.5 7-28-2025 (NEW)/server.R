## server.R - Shiny Server Logic ##


## server.R ##
library(shiny)
library(shinyjs)
library(gstat)
library(sp)
library(ggplot2)
library(leaflet)
library(raster)
library(data.table)
library(dplyr)
library(readxl)
library(deldir)  # NEW

source("R/variogram.R")

# === Shiny Server === #
shinyServer(function(input, output, session) {
  
  # --- Reactive State Flags --- #
  dataReady  <- reactiveVal(FALSE)
  errorMsg   <- reactiveVal(NULL)
  fileLoaded <- reactiveVal(FALSE)
  clearedUI  <- reactiveVal(TRUE)
  analysisRunning <- reactiveVal(FALSE)
  
  # --- UI Reset / File Input Observers --- #
  observeEvent(input$stopBtn, {
    analysisRunning(FALSE)
    shinyjs::hide("stopBtn")
  })
  
  # NEED TO UPDATE: reset state of app to inital state 
  # including all input cleared
  
  observeEvent(input$resetBtn, {
    dataReady(FALSE)
    errorMsg(NULL)
    fileLoaded(FALSE)
    clearedUI(TRUE)
    shinyjs::reset("appForm")
  })
  
  observeEvent(input$file, {
    fileLoaded(TRUE)
    clearedUI(FALSE)
  })
 
  # --- Data Reading and Preprocessing --- # 
  userData <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      switch(ext,
             csv  = fread(input$file$datapath),
             txt  = fread(input$file$datapath),
             xls  = read_excel(input$file$datapath),
             xlsx = read_excel(input$file$datapath),
             stop("Unsupported file type.")
      )
    }, error = function(e) {
      errorMsg("Error reading the uploaded file.")
      NULL
    })
  })
  
  observeEvent(c(userData(), input$valcol), {
    req(userData(), input$valcol)
    med <- median(userData()[[input$valcol]], na.rm = TRUE)
    updateNumericInput(session, "ikThreshold", value = med)
  })
  
  # --- UI Outputs: Upload Message & Errors --- #
  output$fileRequiredMsg <- renderUI({
    if (!fileLoaded()) {
      div(style = "color: red; font-weight: bold; margin-top: 5px;",
          "Please upload file.")
    }
  })
  output$dataWarning <- renderUI({
    req(errorMsg())
    div(style = "color: red; font-weight: bold; margin-bottom: 10px;",
        errorMsg())
  })
  
  # --- Data Preview Table --- #
  output$dataPreview <- renderTable({
    req(userData())
    userData()
  })
  
  # --- UI for Column Selection --- #
  output$columnSelectors <- renderUI({
    req(userData())
    cols <- names(userData())
    tagList(
      selectizeInput("xcol",  "X Coordinate (Longitude/Easting)", choices = c("Please choose variable" = "", cols)),
      selectizeInput("ycol",  "Y Coordinate (Latitude/Northing)", choices = c("Please choose variable" = "", cols)),
      selectizeInput("valcol","Variable",     choices = c("Please choose variable" = "", cols)),
      conditionalPanel(
        condition = "input.krigingType == 'CK'",
        selectizeInput("secondaryVar", "Secondary Variable (Covariate)", choices = c("Please choose variable" = "", cols))
      )
    )
  })
  
  # --- UI Tabs Switching Logic --- #
  output$mainTabs <- renderUI({
    if (clearedUI()) {
      NULL         # removed Data tabPanel
    } else if (!fileLoaded()) {
      tagList(
        div(style = "color:#18536f; font-style:italic; font-weight: bold; margin-bottom:10px;",
            "Please upload a valid dataset"),
        tabsetPanel(tabPanel("Data", tableOutput("dataPreview")))
      )
    } else if (!dataReady()) {
      tabsetPanel(tabPanel("Data", tableOutput("dataPreview")))
    } else {
      tabsetPanel(
        tabPanel("Variogram",
                 uiOutput("variogramNote"),
                 plotOutput("variogramPlot"),
                 div(style = "color: gray; font-size: 13px; margin-top: 5px;",
                     "💾 Right click on the image to copy or save image on your device."),
                 plotOutput("semiVariogramPlot")),
        tabPanel("Kriging",
                 conditionalPanel("!input.quickPreview",
                                  plotOutput("krigingPlot"),
                                  div(style = "color: gray; font-size: 13px; margin-top: 5px;",
                                      "💾 Right click on the image to copy or save image on your device."))),
        tabPanel("Map",
                 conditionalPanel("!input.quickPreview",
                                  leafletOutput("krigingMap", height = 600))),
        tabPanel("Voronoi Diagram", 
                 plotOutput("voronoiPlot", height = 600),
                 div(style = "color: gray; font-size: 13px; margin-top: 5px;",
                     "💾 Right click on the image to copy or save image on your device.")),
        tabPanel("Uploaded Data", tableOutput("dataPreview"))
      )
    }
  })
  
  # --- VGM functions --- #
  getVgmData <- reactive({ 
    df <- cleanAndPrepareData(); req(df)
    vgm <- compute_variogram(df, input$valcol, input$modelType)
    vgm_model <- vgm$model
    vgm_exp <- vgm$exp
    fmla <- vgm$fmla
    
    return(list(
      vgm = vgm,
      vgm_model = vgm_model,
      vgm_exp = vgm_exp,
      fmla = fmla
    ))
  })
  
  output$variogramPlot <- renderPlot({
    vgm_dat <- getVgmData()
    vgm_model <- vgm_dat$vgm_model
    vgm_exp <- vgm_dat$vgm_exp
    fmla <- vgm_dat$fmla
    
    vgm_df <- as.data.frame(vgm_exp)
    varline_df <- variogramLine(vgm_model,
                                maxdist = max(vgm_exp$dist, na.rm = TRUE),
                                n = 200)
    make_variogram_plot(
      vgm_exp, 
      vgm_model, 
      vgm_df, 
      varline_df,
      input$modelType
    )
  })
  
  output$semiVariogramPlot <- renderPlot({
    vgm_dat <- getVgmData()
    vgm_exp <- vgm_dat$vgm_exp

    make_semi_variogram_plot(vgm_exp, input$modelType)
  })

  # --- Main Analysis Triggered by Plot Button --- #
  observeEvent(input$plotBtn, {
    
    # --- Step 1: Input Validation --- #
    errorMsg(NULL)
    if (!fileLoaded()) return()
    
    analysisRunning(TRUE)
    shinyjs::show("stopBtn")
    
    if (is.null(input$xcol) || input$xcol == "") {
      errorMsg("Please choose corresponding data in the X Coordinate"); return()
    }
    if (is.null(input$ycol) || input$ycol == "") {
      errorMsg("Please choose corresponding data in the Y Coordinate"); return()
    }
    if (is.null(input$valcol) || input$valcol == "") {
      errorMsg("Please choose corresponding data in the Variable field"); return()
    }
    
    # --- Step 2: Data Preparation --- #
    df <- cleanAndPrepareData(); req(df)
    
    # --- Step 3: Voronoi Diagram Generation --- #
    output$voronoiPlot <- renderPlot({
      req(df)
      validate(
        need(nrow(df) >= 3, "Need at least 3 points to compute Voronoi diagram.")
      )
      
      df_points <- as.data.frame(df)
      vor <- deldir(df_points[[input$xcol]], df_points[[input$ycol]])
      tiles <- tile.list(vor)
      
      poly_list <- lapply(seq_along(tiles), function(i) {
        tile <- tiles[[i]]
        data.frame(x = tile$x, y = tile$y, id = i)
      })
      
      vor_df <- do.call(rbind, poly_list)
      vor_df <- vor_df %>%
        left_join(df_points %>% mutate(id = row_number()), by = "id")
      
      ggplot(vor_df, aes(x = x, y = y, group = id, fill = .data[[input$valcol]])) +
        geom_polygon(color = "white", linewidth = 0.3) +
        coord_fixed() +
        scale_fill_gradientn(
          colors = c("darkblue", "blue", "skyblue", "lightgreen", "yellow", "orange", "red", "darkred"),
          name = paste(input$valcol)
        ) +
        labs(x = input$xcol, y = input$ycol) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title.x = element_text(face = "bold", color = "black"),
          axis.title.y = element_text(face = "bold", color = "black"),
          axis.text = element_text(color = "black")
        )
    })
    
    # --- Step 4: Threshold Check (for IK) --- #
    if (input$krigingType == "IK") {
      var_vals <- df@data[[input$valcol]]
      thr <- input$ikThreshold
      if (is.null(thr) || is.na(thr) || !is.numeric(thr) || thr < min(var_vals, na.rm = TRUE) || thr > max(var_vals, na.rm = TRUE)) {
        median_val <- median(var_vals, na.rm = TRUE)
        updateNumericInput(session, "ikThreshold", value = median_val)
        showModal(modalDialog(
          title = "Invalid Threshold",
          paste0("Indicator Threshold must be within the range of your variable (", round(min(var_vals), 2), " to ", round(max(var_vals), 2), "). It has been reset to the median (", round(median_val, 2), ")."),
          easyClose = TRUE
        ))
        return()
      }
    }
    
    # --- Step 5 to 11: Variogram, Kriging, Visualization, Export --- #
    withProgress(message = "Running Analysis...", value = 0, {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Running Analysis...", value = 0)
      
      updateProgress <- function(val, stage = "") {
        progress$set(value = val, detail = paste0(stage, " — ", round(val * 100), "%"))
      }
      
      updateProgress(0.1, "Preparing Variogram") # Initial step
      
      vgm_dat <- getVgmData()
      vgm_model <- vgm_dat$vgm_model
      vgm_exp <- vgm_dat$vgm_exp
      fmla <- vgm_dat$fmla
      
      req(analysisRunning())  # Stop if user interrupted
      
      # Check if the fitted variogram model has invalid (negative) range values
      if (any(vgm_model$range < 0, na.rm = TRUE)) {
        cat("❌ Variogram fit error: Negative range(s) detected:\n")
        print(vgm_model)
        
        showModal(modalDialog(
          title = "Invalid Variogram Fit",
          "The fitted variogram model has a negative range, which is not physically valid. Please try a different model function (e.g., 'Spherical') or verify your input data.",
          easyClose = TRUE
        ))
        return()
      }
      
      output$variogramNote <- renderUI({
        if (any(is.na(vgm_model$psill))) {
          div(style = "color: orange; padding-bottom: 5px;",
              "⚠️ Variogram didn’t fully converge; results may be approximate.")
        }
      })
      
      updateProgress(0.4, "Fitting Variogram")
      updateProgress(0.6, "Running Kriging")
      updateProgress(1, "Finalizing")
      
      uiOutput("variogramPlot")
      uiOutput("semiVariogramPlot")
      
      if (input$quickPreview) {
        incProgress(1); return()
      }
      
      incProgress(0.6, "Running Kriging")
      xr <- seq(bbox(df)[1,1], bbox(df)[1,2], length.out = input$gridResolution)
      yr <- seq(bbox(df)[2,1], bbox(df)[2,2], length.out = input$gridResolution)
      grid_df <- setNames(expand.grid(xr, yr), c(input$xcol, input$ycol))
      coordinates(grid_df) <- c(input$xcol, input$ycol)
      proj4string(grid_df) <- CRS(proj4string(df))
      
      kriged <- tryCatch({
        switch(input$krigingType,
               "OK" = krige(fmla, df, grid_df, model = vgm_model),
               "SK" = krige(fmla, df, grid_df, model = vgm_model,
                            beta = mean(df@data[[input$valcol]], na.rm = TRUE)),
               "UK" = krige(as.formula(paste(input$valcol, "~", input$xcol, "+", input$ycol)), df, grid_df, model = vgm_model),
               
               "IK" = {
                 ...
                 krige(indVar ~ 1, df_ind, grid_df, model = vgm_model_ind)
               },
               
               "CK" = {
                 ...
                 v <- tryCatch(variogram(g), error = function(e) {
                   cat("❌ Co-kriging variogram() error:", conditionMessage(e), "\n")
                   showModal(modalDialog("Error calculating experimental variograms.")); return(NULL)
                 })
                 req(v)
                 
                 fitted_model <- tryCatch({
                   fit.lmc(v, g, vgm(model = input$modelType, psill = 1,
                                     range = diff(bbox(df)[1,]), nugget = 0))
                 }, error = function(e) {
                   cat("❌ Co-kriging fit.lmc() error:", conditionMessage(e), "\n")
                   showModal(modalDialog("Failed to fit cross-variogram. Try different variables or a simpler model.")); return(NULL)
                 })
                 req(fitted_model)
                 
                 tryCatch({
                   predict(fitted_model, grid_df)
                 }, error = function(e) {
                   cat("❌ Co-kriging predict() error:", conditionMessage(e), "\n")
                   showModal(modalDialog("Co-Kriging prediction failed.")); return(NULL)
                 })
               }
        )
      }, error = function(e) {
        cat("❌ General kriging error:", conditionMessage(e), "\n")
        showModal(modalDialog(
          title = "Incompatible Configuration",
          "The selected Kriging method and Variogram model function are incompatible with the dataset or failed to compute. Please try a different combination.",
          easyClose = TRUE
        ))
        return(NULL)
      })
      
      ddf <- as.data.frame(kriged)
      
      if (nrow(ddf) == 0 || ncol(ddf) < 3) {
        showModal(modalDialog(
          title = "Kriging Failed",
          "No valid predictions could be made. This is often due to invalid data input, poor variogram fit, or extrapolation outside the data extent.",
          easyClose = TRUE
        ))
        cat("❌ Kriging output is empty or invalid\n")
        return()
      }
      
      names(ddf)[1:2] <- c("x", "y")
      vals <- ddf$var1.pred
      
      if (all(is.na(vals)) || length(vals) == 0) {
        showModal(modalDialog(
          title = "Kriging Prediction Failed",
          "No valid predictions could be made. Please check your input data or variogram fit."
        ))
        return()
      }
      
      customPalette <- colorNumeric(
        palette = c("#6e40aa", "#4575b4", "#91bfdb", "#e0f3f8",
                    "#ffffbf", "#fee090", "#fc8d59", "#d73027"),
        domain = rev(range(vals, na.rm = TRUE)),
        na.color = "transparent"
      )
      output$krigingPlot <- renderPlot({
        ggplot(ddf, aes(x = x, y = y, fill = var1.pred)) +
          geom_tile() + coord_equal() +
          scale_fill_gradientn(
            colors = c("#6e40aa", "#4575b4", "#91bfdb", "#e0f3f8",
                       "#ffffbf", "#fee090", "#fc8d59", "#d73027"),
            name = "Prediction",
            guide = guide_colorbar(
              barwidth = 1, barheight = 15,
              ticks.colour = "black", ticks.linewidth = 1,
              label.theme = element_text(color = "black"),
              frame.colour = "black"
            )
          ) +
          labs(title = "Kriging Prediction", x = "X", y = "Y") +
          theme_minimal()
      })
      
      r <- rasterFromXYZ(ddf[, c("x", "y", "var1.pred")])
      crs(r) <- CRS(proj4string(df))
      output$krigingMap <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$OpenStreetMap, group = "Roadmap") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
          addProviderTiles(providers$Esri.WorldTopoMap, group = "Topography") %>%
          addRasterImage(r, colors = customPalette, opacity = 0.7, group = "Kriging") %>%
          addLegend(pal = customPalette, values = vals, title = "Kriging",
                    position = "topright", opacity = 1,
                    labFormat = labelFormat(prefix = "", digits = 2)) %>%
          addLayersControl(
            baseGroups = c("Roadmap", "Satellite", "Topography"),
            overlayGroups = c("Kriging"),
            options = layersControlOptions(collapsed = FALSE)
          )
      })
      
      incProgress(1, "Done")
      analysisRunning(FALSE)
      shinyjs::hide("stopBtn")
      updateProgress(1)  # 100%
    })
    
    # --- Step 12: Rose Diagram (if direction column provided) --- #
    output$rosePlot <- renderPlot({
      req(userData(), input$dircol)
      df <- as.data.frame(userData())
      
      # Validate direction column
      if (!input$dircol %in% names(df)) return()
      df[[input$dircol]] <- as.numeric(df[[input$dircol]])
      df <- df[!is.na(df[[input$dircol]]), ]
      
      validate(
        need(nrow(df) > 2, "Not enough valid directional data to draw rose diagram.")
      )
      
      ggplot(df, aes(x = .data[[input$dircol]])) +
        geom_histogram(binwidth = 10, fill = "steelblue", color = "black", boundary = 0, closed = "left") +
        coord_polar(start = 0, direction = 1) +
        theme_minimal() +
        labs(title = "Rose Diagram",
             x = "Direction (°)", y = "Frequency") +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black")
        )
    })
  })
  
  # --- Data Cleaning & Spatial Preparation --- #
  cleanAndPrepareData <- reactive({
    req(userData(), input$xcol, input$ycol, input$valcol)
    
    # Make sure all selected variables are distinct
    if (length(unique(c(input$xcol, input$ycol, input$valcol))) < 3) {
      errorMsg("X, Y and Variable columns must be distinct.")
      dataReady(FALSE)
      return(NULL)
    }
    
    df <- userData()
    
    # Keep secondary variable if selected (for CK)
    vars <- unique(c(input$xcol, input$ycol, input$valcol, input$secondaryVar))
    vars <- vars[vars != ""]
    df <- df[, vars, with = FALSE]
    
    # Force numeric conversion
    for (v in vars) {
      df[[v]] <- as.numeric(df[[v]])
    }
    
    # Drop NA and aggregate by coordinates
    df <- na.omit(df)
    df <- df %>%
      group_by_at(c(input$xcol, input$ycol)) %>%
      summarise(across(all_of(setdiff(vars, c(input$xcol, input$ycol))), mean), .groups = "drop")
    
    # Apply jitter
    set.seed(42)
    df[[input$xcol]] <- jitter(df[[input$xcol]], factor = 1e-4)
    df[[input$ycol]] <- jitter(df[[input$ycol]], factor = 1e-4)
    
    # Convert to Spatial
    coordinates(df) <- c(input$xcol, input$ycol)
    
    if (input$coordType == "longlat") {
      if (any(coordinates(df)[,1] < -180 | coordinates(df)[,1] > 180) ||
          any(coordinates(df)[,2] < -90  | coordinates(df)[,2] >  90)) {
        showModal(modalDialog(title = "Invalid Coordinates",
                              "Please upload real geographic data."))
        dataReady(FALSE)
        return(NULL)
      }
      proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
    } else {
      proj4string(df) <- CRS("+proj=utm +zone=33 +datum=WGS84")  # Update zone as needed
    }
    
    errorMsg(NULL)
    dataReady(TRUE)
    df
  })

  # --- JS Drag & Drop Styling --- #
  observe({
    runjs("document.getElementById('file').addEventListener('dragover', function(e) {e.preventDefault(); this.style.border='2px dashed #18536f';});")
    runjs("document.getElementById('file').addEventListener('dragleave', function(e) {this.style.border='none';});")
  })

})