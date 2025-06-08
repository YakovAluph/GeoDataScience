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

shinyServer(function(input, output, session) {
  
  dataReady  <- reactiveVal(FALSE)
  errorMsg   <- reactiveVal(NULL)
  fileLoaded <- reactiveVal(FALSE)
  clearedUI  <- reactiveVal(TRUE)
  
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
  
  output$dataPreview <- renderTable({
    req(userData())
    userData()
  })
  
  output$columnSelectors <- renderUI({
    req(userData())
    cols <- names(userData())
    tagList(
      selectizeInput("xcol",  "X Coordinate", choices = c("Please choose variable" = "", cols)),
      selectizeInput("ycol",  "Y Coordinate", choices = c("Please choose variable" = "", cols)),
      selectizeInput("valcol","Variable",     choices = c("Please choose variable" = "", cols))
    )
  })
  
  output$mainTabs <- renderUI({
    if (clearedUI()) {
      tabsetPanel(tabPanel("Data"))
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
        tabPanel("Variogram", uiOutput("variogramNote"), plotOutput("variogramPlot"), plotOutput("semiVariogramPlot")),
        tabPanel("Kriging",
                 conditionalPanel("!input.quickPreview",
                                  plotOutput("krigingPlot"),
                                  div(class = "download-button",
                                      downloadButton("downloadKriging", "Download Kriging CSV")))),
        tabPanel("Map",
                 conditionalPanel("!input.quickPreview",
                                  leafletOutput("krigingMap", height = 600))),
        tabPanel("Uploaded Data", tableOutput("dataPreview"))
      )
    }
  })
  
  observeEvent(input$plotBtn, {
    errorMsg(NULL)
    if (!fileLoaded()) return()
    
    if (is.null(input$xcol) || input$xcol == "") {
      errorMsg("Please choose corresponding data in the X Coordinate"); return()
    }
    if (is.null(input$ycol) || input$ycol == "") {
      errorMsg("Please choose corresponding data in the Y Coordinate"); return()
    }
    if (is.null(input$valcol) || input$valcol == "") {
      errorMsg("Please choose corresponding data in the Variable field"); return()
    }
    
    df <- cleanAndPrepareData(); req(df)
    
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
    
    withProgress(message = "Running Analysis...", value = 0.1, {
      fmla <- as.formula(paste(input$valcol, "~ 1"))
      vgm_exp <- variogram(fmla, df)
      initial <- vgm(psill = var(df[[input$valcol]], na.rm=TRUE),
                     model = input$modelType,
                     range = diff(bbox(df)[1,]), nugget = 0)
      vgm_model <- suppressWarnings(
        fit.variogram(vgm_exp, model = initial,
                      fit.sills = TRUE, fit.ranges = TRUE, fit.kappa = TRUE)
      )
      
      vgm_df <- as.data.frame(vgm_exp)
      varline_df <- variogramLine(vgm_model,
                                  maxdist = max(vgm_exp$dist, na.rm = TRUE),
                                  n = 200)
      
      output$variogramNote <- renderUI({
        if (any(is.na(vgm_model$psill))) {
          div(style = "color: orange; padding-bottom: 5px;",
              "⚠️ Variogram didn’t fully converge; results may be approximate.")
        }
      })
      
      incProgress(0.3, "Fitting Variogram")
      
      output$variogramPlot <- renderPlot({
        ggplot() +
          geom_point(data = vgm_df, aes(dist, gamma), shape=16, size=3) +
          geom_line(data = varline_df, aes(dist, gamma), linewidth=1) +
          labs(title = paste0("Variogram (", input$modelType, ")"),
               x = "Lag distance", y = "Semivariance") +
          theme_minimal() +
          theme(
            panel.border = element_rect(color="black", fill=NA, linewidth=1),
            plot.title = element_text(face="bold", size=18, hjust=0.5),
            axis.title = element_text(size=14),
            axis.text = element_text(size=12)
          ) +
          scale_x_continuous(expand=c(0,0), limits=c(0,NA)) +
          scale_y_continuous(expand=c(0,0), limits=c(0,NA))
      })
      
      output$semiVariogramPlot <- renderPlot({
        ggplot(vgm_exp, aes(dist, gamma)) +
          geom_point(shape=16, size=3) +
          geom_line(linewidth=1) +
          labs(title = paste0("Semi-Variogram (", input$modelType, ")"),
               x = "Lag distance", y = "Semivariance") +
          theme_minimal() +
          theme(
            panel.border = element_rect(color="black", fill=NA, linewidth=1),
            plot.title = element_text(face="bold", size=18, hjust=0.5),
            axis.title = element_text(size=14),
            axis.text = element_text(size=12)
          ) +
          scale_x_continuous(expand=c(0,0), limits=c(0,NA)) +
          scale_y_continuous(expand=c(0,0), limits=c(0,NA))
      })
      
      if (input$quickPreview) {
        incProgress(1); return()
      }
      
      incProgress(0.6, "Running Kriging")
      xr <- seq(bbox(df)[1,1], bbox(df)[1,2], length.out = input$gridResolution)
      yr <- seq(bbox(df)[2,1], bbox(df)[2,2], length.out = input$gridResolution)
      grid_df <- setNames(expand.grid(xr, yr), c(input$xcol, input$ycol))
      coordinates(grid_df) <- c(input$xcol, input$ycol)
      proj4string(grid_df) <- CRS(proj4string(df))
      
      kriged <- switch(input$krigingType,
                       "OK" = krige(fmla, df, grid_df, model = vgm_model),
                       "SK" = krige(fmla, df, grid_df, model = vgm_model, beta = mean(df@data[[input$valcol]], na.rm = TRUE)),
                       "UK" = krige(as.formula(paste(input$valcol, "~", input$xcol, "+", input$ycol)), df, grid_df, model = vgm_model),
                       "IK" = {
                         df_ind <- df
                         df_ind@data$indVar <- as.numeric(df_ind@data[[input$valcol]] > input$ikThreshold)
                         vgm_exp_ind <- variogram(indVar~1, df_ind)
                         initial_ind <- vgm(psill = var(df_ind@data$indVar, na.rm=TRUE),
                                            model = input$modelType,
                                            range = diff(bbox(df)[1,]), nugget = 0)
                         vgm_model_ind <- suppressWarnings(
                           fit.variogram(vgm_exp_ind, model = initial_ind,
                                         fit.sills = TRUE, fit.ranges = TRUE, fit.kappa = TRUE)
                         )
                         krige(indVar~1, df_ind, grid_df, model = vgm_model_ind)
                       }
      )
      
      ddf <- as.data.frame(kriged)
      names(ddf)[1:2] <- c("x", "y")
      vals <- ddf$var1.pred  # Invert values
      customPalette <- colorNumeric(
        palette = c("#6e40aa", "#4575b4", "#91bfdb", "#e0f3f8",
                    "#ffffbf", "#fee090", "#fc8d59", "#d73027"),
        domain = rev(range(vals, na.rm = TRUE)),  # 🔁 REVERSED domain
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
      
      output$downloadKriging <- downloadHandler(
        filename = function() {
          paste0("kriging_results_", input$krigingType, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(ddf, file, row.names = FALSE)
        }
      )
      
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
    })
  })
  
  cleanAndPrepareData <- reactive({
    req(userData(), input$xcol, input$ycol, input$valcol)
    if (length(unique(c(input$xcol, input$ycol, input$valcol))) < 3) {
      errorMsg("X, Y and Variable columns must be distinct."); dataReady(FALSE); return(NULL)
    }
    df <- userData()
    df[[input$xcol]]   <- as.numeric(df[[input$xcol]])
    df[[input$ycol]]   <- as.numeric(df[[input$ycol]])
    df[[input$valcol]] <- as.numeric(df[[input$valcol]])
    df <- na.omit(df[, c(input$xcol, input$ycol, input$valcol), with = FALSE])
    df <- df %>%
      group_by_at(c(input$xcol, input$ycol)) %>%
      summarise(across(all_of(input$valcol), mean), .groups = "drop")
    set.seed(42)
    df[[input$xcol]] <- jitter(df[[input$xcol]], factor = 1e-4)
    df[[input$ycol]] <- jitter(df[[input$ycol]], factor = 1e-4)
    coordinates(df) <- c(input$xcol, input$ycol)
    
    if (input$coordType == "longlat") {
      if (any(coordinates(df)[,1] < -180 | coordinates(df)[,1] > 180) ||
          any(coordinates(df)[,2] < -90  | coordinates(df)[,2] >  90)) {
        showModal(modalDialog(title = "Invalid Coordinates",
                              "Please upload real geographic data.")); dataReady(FALSE); return(NULL)
      }
      proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
    } else {
      proj4string(df) <- CRS("+proj=utm +zone=33 +datum=WGS84")
    }
    
    errorMsg(NULL); dataReady(TRUE); df
  })
  
  observe({
    runjs("document.getElementById('file').addEventListener('dragover', function(e) {e.preventDefault(); this.style.border='2px dashed #18536f';});")
    runjs("document.getElementById('file').addEventListener('dragleave', function(e) {this.style.border='none';});")
  })
  
})