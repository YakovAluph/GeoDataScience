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
  
  # Flags & error storage
  dataReady <- reactiveVal(FALSE)
  errorMsg  <- reactiveVal(NULL)
  
  # Reset logic
  observeEvent(input$resetBtn, {
    dataReady(FALSE)
    errorMsg(NULL)
    reset("file")
    
    # Clear all outputs
    output$fileRequiredMsg      <- renderUI(NULL)
    output$dataWarning          <- renderUI(NULL)
    output$dataPreview          <- renderTable({})
    output$columnSelectors      <- renderUI(NULL)
    output$variogramNote        <- renderUI(NULL)
    output$variogramPlot        <- renderPlot({})
    output$semiVariogramPlot    <- renderPlot({})
    output$downloadVariogram    <- downloadHandler(NULL, NULL)
    output$krigingPlot          <- renderPlot({})
    output$downloadKriging      <- downloadHandler(NULL, NULL)
    output$krigingMap           <- renderLeaflet({})
    output$cvPlot               <- renderPlot({})
    output$downloadCV           <- downloadHandler(NULL, NULL)
    output$cvBox                <- renderUI(NULL)
    output$mainTabs             <- renderUI(NULL)
  })
  
  # Load uploaded data
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
  
  # “Please upload file.” message
  output$fileRequiredMsg <- renderUI({
    if (is.null(input$file)) {
      div(style = "color: red; font-weight: bold; margin-top: 5px;",
          "Please upload file.")
    }
  })
  
  # X/Y/Variable distinct‐columns error
  output$dataWarning <- renderUI({
    req(errorMsg())
    div(style = "color: red; font-weight: bold; margin-bottom: 10px;",
        errorMsg())
  })
  
  # Data preview
  output$dataPreview <- renderTable({
    req(userData())
    userData()
  })
  
  # Column selectors
  output$columnSelectors <- renderUI({
    req(userData())
    cols <- names(userData())
    tagList(
      selectInput("xcol",   "X Coordinate", choices = cols),
      selectInput("ycol",   "Y Coordinate", choices = cols),
      selectInput("valcol", "Variable",     choices = cols)
    )
  })
  
  # CV prompt/button
  output$cvBox <- renderUI({
    if (dataReady()) {
      wellPanel(
        p("✅ Analysis complete!"),
        strong("Would you like to cross-validate your data for sample accuracy?"),
        actionButton("cvBtn", "Run Cross-Validation", class = "btn btn-custom")
      )
    }
  })
  
  # Dynamic tabs UI
  output$mainTabs <- renderUI({
    if (!dataReady()) {
      tagList(
        div(style = "color:#18536f; font-style:italic; margin-bottom:10px;",
            "Please upload a valid dataset and click \"Run Analysis\" to view the other tabs."),
        tabsetPanel(
          tabPanel("Data", tableOutput("dataPreview"))
        )
      )
    } else {
      tabsetPanel(
        tabPanel("Data", tableOutput("dataPreview")),
        tabPanel("Variogram",
                 uiOutput("variogramNote"),
                 plotOutput("variogramPlot"),
                 plotOutput("semiVariogramPlot"),
                 div(class = "download-button",
                     downloadButton("downloadVariogram", "Download Variogram (JPEG)"))
        ),
        tabPanel("Kriging",
                 conditionalPanel("!input.quickPreview",
                                  plotOutput("krigingPlot"),
                                  div(class = "download-button",
                                      downloadButton("downloadKriging","Download Kriging CSV"))
                 )
        ),
        tabPanel("Map",
                 conditionalPanel("!input.quickPreview",
                                  leafletOutput("krigingMap", height = 600)
                 )
        ),
        tabPanel("Cross-Validation",
                 conditionalPanel("!input.quickPreview",
                                  plotOutput("cvPlot"),
                                  div(class = "download-button",
                                      downloadButton("downloadCV","Download CV Results"))
                 )
        )
      )
    }
  })
  
  # Clean and prepare spatial data
  cleanAndPrepareData <- reactive({
    req(userData(), input$xcol, input$ycol, input$valcol)
    
    # Ensure X, Y, and Variable are distinct
    if (length(unique(c(input$xcol, input$ycol, input$valcol))) < 3) {
      errorMsg("X, Y and Variable columns must be distinct.")
      dataReady(FALSE)
      return(NULL)
    }
    
    df <- userData()
    df[[input$xcol]] <- as.numeric(df[[input$xcol]])
    df[[input$ycol]] <- as.numeric(df[[input$ycol]])
    df[[input$valcol]] <- as.numeric(df[[input$valcol]])
    df <- na.omit(df[, c(input$xcol, input$ycol, input$valcol), with = FALSE])
    
    df <- df %>%
      group_by_at(c(input$xcol, input$ycol)) %>%
      summarise(across(all_of(input$valcol), mean), .groups = "drop")
    
    set.seed(42)
    df[[input$xcol]] <- jitter(df[[input$xcol]], factor = 1e-4)
    df[[input$ycol]] <- jitter(df[[input$ycol]], factor = 1e-4)
    
    coordinates(df) <- c(input$xcol, input$ycol)
    xvals <- coordinates(df)[,1]; yvals <- coordinates(df)[,2]
    
    if (input$coordType == "longlat") {
      if (any(xvals < -180 | xvals > 180, na.rm = TRUE) ||
          any(yvals < -90 | yvals > 90, na.rm = TRUE)) {
        showModal(modalDialog(
          title = "Invalid Coordinates",
          "Please upload real geographic data (Lon ∈ [–180,180], Lat ∈ [–90,90]).",
          easyClose = TRUE
        ))
        dataReady(FALSE)
        return(NULL)
      }
      proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
    } else {
      if (any(!is.finite(xvals)) || any(!is.finite(yvals))) {
        showModal(modalDialog(
          title = "Invalid Coordinates",
          "X/Y must be numeric projected coordinates (e.g. UTM).",
          easyClose = TRUE
        ))
        dataReady(FALSE)
        return(NULL)
      }
      proj4string(df) <- CRS("+proj=utm +zone=33 +datum=WGS84")
    }
    
    errorMsg(NULL)
    dataReady(TRUE)
    df
  })
  
  # Run Analysis: Variogram & Kriging
  observeEvent(input$plotBtn, {
    errorMsg(NULL)
    if (is.null(input$file)) return()
    
    df <- cleanAndPrepareData()
    req(df)
    
    withProgress(message = "Running Analysis...", value = 0.1, {
      
      # 1) Variogram
      fmla    <- as.formula(paste(input$valcol, "~ 1"))
      vgm_exp <- variogram(fmla, df)
      
      initial <- vgm(
        psill = var(df[[input$valcol]], na.rm = TRUE),
        model = input$modelType,
        range = diff(bbox(df)[1,]),
        nugget = 0
      )
      
      vgm_model <- suppressWarnings(
        fit.variogram(vgm_exp,
                      model      = initial,
                      fit.sills  = TRUE,
                      fit.ranges = TRUE,
                      fit.kappa  = TRUE)
      )
      
      # Inline variogram convergence note
      output$variogramNote <- renderUI({
        if (any(is.na(vgm_model$psill))) {
          div(style = "color: orange; padding-bottom: 5px;",
              "⚠️ Variogram didn’t fully converge; results may be approximate.")
        }
      })
      
      incProgress(0.3, "Fitting Variogram")
      
      # Updated variogramPlot
      output$variogramPlot <- renderPlot({
        maxDist  <- max(vgm_exp$dist,  na.rm = TRUE)
        maxGamma <- max(vgm_exp$gamma, na.rm = TRUE)
        plot(
          vgm_exp, model = vgm_model,
          main     = paste("Variogram -", input$modelType),
          xlab     = "Lab distance",
          ylab     = "Semivariance",
          xlim     = c(0, maxDist),
          ylim     = c(0, maxGamma),
          pch      = 16,
          col      = "black",
          col.line = "black",
          lwd      = 2,
          cex.lab  = 1.5,
          font.lab = 2
        )
      })
      
      # Updated semiVariogramPlot
      output$semiVariogramPlot <- renderPlot({
        ggplot(vgm_exp, aes(x = dist, y = gamma)) +
          geom_point(shape = 16, size = 3, color = "black") +
          geom_line(size = 1, color = "black") +
          labs(
            title = paste("Semi-Variogram (", input$modelType, ")"),
            x     = "Lab distance",
            y     = "Semivariance"
          ) +
          theme_minimal() +
          theme(
            axis.title.x = element_text(face = "bold", size = 14),
            axis.title.y = element_text(face = "bold", size = 14),
            axis.text    = element_text(size = 12)
          ) +
          scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
      })
      
      output$downloadVariogram <- downloadHandler(
        filename = function() {
          paste0("variogram_plot_", input$modelType, "_", Sys.Date(), ".jpeg")
        },
        content = function(file) {
          jpeg(file, width = 800, height = 600)
          maxDist  <- max(vgm_exp$dist,  na.rm = TRUE)
          maxGamma <- max(vgm_exp$gamma, na.rm = TRUE)
          plot(
            vgm_exp, model = vgm_model,
            main     = paste("Variogram -", input$modelType),
            xlab     = "Lab distance",
            ylab     = "Semivariance",
            xlim     = c(0, maxDist),
            ylim     = c(0, maxGamma),
            pch      = 16,
            col      = "black",
            col.line = "black",
            lwd      = 2,
            cex.lab  = 1.5,
            font.lab = 2
          )
          dev.off()
        }
      )
      
      # 2) Skip Kriging on quickPreview
      if (input$quickPreview) {
        incProgress(1)
        return()
      }
      
      # 3) Kriging
      incProgress(0.6, "Running Kriging")
      grid_res <- input$gridResolution
      b        <- bbox(df)
      xr       <- seq(b[1,1], b[1,2], length.out = grid_res)
      yr       <- seq(b[2,1], b[2,2], length.out = grid_res)
      grid     <- expand.grid(x = xr, y = yr)
      coordinates(grid) <- ~ x + y
      proj4string(grid) <- CRS(proj4string(df))
      
      kriged <- krige(fmla, df, grid, model = vgm_model)
      
      vals <- kriged$var1.pred
      pal  <- colorNumeric("viridis",
                           domain   = range(vals, na.rm = TRUE),
                           na.color = "transparent")
      
      output$krigingPlot <- renderPlot({
        ddf <- as.data.frame(kriged)
        ggplot(ddf, aes(x, y, fill = var1.pred)) +
          geom_tile() +
          coord_equal() +
          scale_fill_viridis_c(name = "Prediction") +
          labs(title = "Kriging Prediction", x = "X", y = "Y") +
          theme_minimal()
      })
      
      output$downloadKriging <- downloadHandler(
        filename = function() {
          paste0("kriging_results_", input$modelType, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(as.data.frame(kriged), file, row.names = FALSE)
        }
      )
      
      # 4) Leaflet map
      r <- rasterFromXYZ(as.data.frame(kriged)[, c("x", "y", "var1.pred")])
      crs(r) <- CRS(proj4string(df))
      output$krigingMap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addRasterImage(r, colors = pal, opacity = 0.7) %>%
          addLegend(pal = pal, values = vals, title = "Kriging")
      })
      
      incProgress(1, "Done")
    })
  })
  
  # Cross-Validation (unchanged)
  observeEvent(input$cvBtn, {
    req(dataReady())
    df <- cleanAndPrepareData()
    req(df)
    
    withProgress(message = "Cross-validating...", value = 0, {
      n    <- nrow(df)
      pred <- numeric(n)
      obs  <- numeric(n)
      fmla <- as.formula(paste(input$valcol, "~ 1"))
      vgm_model <- fit.variogram(
        variogram(fmla, df),
        model = vgm(input$modelType)
      )
      
      for (i in 1:n) {
        incProgress(1/n, detail = paste("Point", i, "of", n))
        td   <- df[-i, ]
        test <- df[i, , drop = FALSE]
        res  <- tryCatch({
          krige(fmla, td, test, model = vgm_model)
        }, error = function(e) NULL)
        pred[i] <- if (!is.null(res)) res$var1.pred else NA
        obs [i] <- test[[input$valcol]]
      }
      
      output$cvPlot <- renderPlot({
        dfc <- data.frame(Observed = obs, Predicted = pred)
        ggplot(dfc, aes(Observed, Predicted)) +
          geom_point(color = "blue") +
          geom_abline(slope = 1, intercept = 0,
                      color = "red", linetype = "dashed") +
          labs(title = "Cross-Validation: Observed vs Predicted") +
          theme_minimal()
      })
      
      output$downloadCV <- downloadHandler(
        filename = function() {
          paste0("cv_results_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(data.frame(Observed = obs, Predicted = pred),
                    file, row.names = FALSE)
        }
      )
    })
  })
  
})
