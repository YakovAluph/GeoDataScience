## server.r ##
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
  dataReady  <- reactiveVal(FALSE)
  errorMsg   <- reactiveVal(NULL)
  fileLoaded <- reactiveVal(FALSE)
  
  # Reset logic
  observeEvent(input$resetBtn, {
    dataReady(FALSE)
    errorMsg(NULL)
    fileLoaded(FALSE)
    shinyjs::reset("appForm")
  })
  
  # Track uploads
  observeEvent(input$file, {
    fileLoaded(TRUE)
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
  
  # auto‐set the IK threshold to the median of the chosen variable
  observeEvent(c(userData(), input$valcol), {
    req(userData(), input$valcol)
    med <- median(userData()[[input$valcol]], na.rm = TRUE)
    updateNumericInput(session, "ikThreshold", value = med)
  })
  
  # “Please upload file.” message
  output$fileRequiredMsg <- renderUI({
    if (!fileLoaded()) {
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
  
  # Column selectors with placeholder text
  output$columnSelectors <- renderUI({
    req(userData())
    cols <- names(userData())
    tagList(
      selectizeInput("xcol",  "X Coordinate", choices = cols, selected = NULL,
                     options = list(placeholder = "Select a column",
                                    onInitialize = I('function() { this.setValue(""); }'))),
      selectizeInput("ycol",  "Y Coordinate", choices = cols, selected = NULL,
                     options = list(placeholder = "Select a column",
                                    onInitialize = I('function() { this.setValue(""); }'))),
      selectizeInput("valcol","Variable",       choices = cols, selected = NULL,
                     options = list(placeholder = "Select a column",
                                    onInitialize = I('function() { this.setValue(""); }')))
    )
  })
  
  # Dynamic tabs UI
  output$mainTabs <- renderUI({
    if (!fileLoaded()) return(NULL)
    if (!dataReady()) {
      tagList(
        div(style = "color:#18536f; font-style:italic; margin-bottom:10px;",
            "Please upload a valid dataset and click \"Run Analysis\" to view the other tabs."),
        tabsetPanel(tabPanel("Data", tableOutput("dataPreview")))
      )
    } else {
      tabsetPanel(
        tabPanel("Data",      tableOutput("dataPreview")),
        tabPanel("Variogram", uiOutput("variogramNote"), plotOutput("variogramPlot"), plotOutput("semiVariogramPlot")),
        tabPanel("Kriging",
                 conditionalPanel("!input.quickPreview",
                                  plotOutput("krigingPlot"),
                                  div(class = "download-button",
                                      downloadButton("downloadKriging", "Download Kriging CSV"))
                 )
        ),
        tabPanel("Map",
                 conditionalPanel("!input.quickPreview",
                                  leafletOutput("krigingMap", height = 600))
        )
      )
    }
  })
  
  # Clean and prepare spatial data
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
  
  # Run Analysis: Variogram & Kriging
  observeEvent(input$plotBtn, {
    errorMsg(NULL)
    if (!fileLoaded()) return()
    df <- cleanAndPrepareData(); req(df)
    
    withProgress(message = "Running Analysis...", value = 0.1, {
      # Variogram fit
      fmla    <- as.formula(paste(input$valcol, "~ 1"))
      vgm_exp <- variogram(fmla, df)
      initial <- vgm(psill = var(df[[input$valcol]], na.rm=TRUE),
                     model = input$modelType,
                     range = diff(bbox(df)[1,]), nugget = 0)
      vgm_model <- suppressWarnings(
        fit.variogram(vgm_exp, model = initial,
                      fit.sills = TRUE, fit.ranges = TRUE, fit.kappa = TRUE)
      )
      
      # prepare data for ggplot in Variogram panels
      vgm_df     <- as.data.frame(vgm_exp)
      varline_df <- variogramLine(vgm_model,
                                  maxdist = max(vgm_exp$dist, na.rm = TRUE),
                                  n       = 200)
      
      output$variogramNote <- renderUI({
        if (any(is.na(vgm_model$psill))) {
          div(style = "color: orange; padding-bottom: 5px;",
              "⚠️ Variogram didn’t fully converge; results may be approximate.")
        }
      })
      
      incProgress(0.3, "Fitting Variogram")
      
      # Variogram plot
      output$variogramPlot <- renderPlot({
        ggplot() +
          geom_point(data = vgm_df,    aes(dist, gamma), shape=16, size=3) +
          geom_line(data  = varline_df,aes(dist, gamma), size=1) +
          labs(title = paste0("Variogram (", input$modelType, ")"),
               x = "Lag distance", y = "Semivariance") +
          theme_minimal() +
          theme(
            panel.border    = element_rect(color="black", fill=NA, size=1),
            plot.title      = element_text(face="bold", size=18, hjust=0.5),
            axis.title      = element_text(size=14),
            axis.text       = element_text(size=12)
          ) +
          scale_x_continuous(expand=c(0,0), limits=c(0,NA)) +
          scale_y_continuous(expand=c(0,0), limits=c(0,NA))
      })
      
      # Semi-variogram plot
      output$semiVariogramPlot <- renderPlot({
        ggplot(vgm_exp, aes(dist, gamma)) +
          geom_point(shape=16, size=3) +
          geom_line(linewidth=1) +
          labs(title = paste0("Semi-Variogram (", input$modelType, ")"),
               x = "Lag distance", y = "Semivariance") +
          theme_minimal() +
          theme(
            panel.border    = element_rect(color="black", fill=NA, linewidth=1),
            plot.title      = element_text(face="bold", size=18, hjust=0.5),
            axis.title      = element_text(size=14),
            axis.text       = element_text(size=12)
          ) +
          scale_x_continuous(expand=c(0,0), limits=c(0,NA)) +
          scale_y_continuous(expand=c(0,0), limits=c(0,NA))
      })
      
      # Skip kriging if quickPreview
      if (input$quickPreview) {
        incProgress(1); return()
      }
      
      # ---- build prediction grid with correct column names ----
      incProgress(0.6, "Running Kriging")
      xr <- seq(bbox(df)[1,1], bbox(df)[1,2], length.out = input$gridResolution)
      yr <- seq(bbox(df)[2,1], bbox(df)[2,2], length.out = input$gridResolution)
      grid_df <- setNames(expand.grid(xr, yr), c(input$xcol, input$ycol))
      coordinates(grid_df) <- c(input$xcol, input$ycol)
      proj4string(grid_df) <- CRS(proj4string(df))
      
      # choose kriging method
      if (input$krigingType == "OK") {
        kriged <- krige(fmla, df, grid_df, model = vgm_model)
      } else if (input$krigingType == "SK") {
        meanVal <- mean(df@data[[input$valcol]], na.rm = TRUE)
        kriged  <- krige(fmla, df, grid_df, model = vgm_model, beta = meanVal)
      } else if (input$krigingType == "UK") {
        fmla_uk <- as.formula(paste(input$valcol, "~", input$xcol, "+", input$ycol))
        kriged  <- krige(fmla_uk, df, grid_df, model = vgm_model)
      } else {
        # Indicator Kriging
        thr    <- input$ikThreshold
        df_ind <- df
        df_ind@data$indVar <- as.numeric(df_ind@data[[input$valcol]] > thr)
        vgm_exp_ind <- variogram(indVar~1, df_ind)
        initial_ind <- vgm(psill = var(df_ind@data$indVar, na.rm=TRUE),
                           model = input$modelType,
                           range = diff(bbox(df)[1,]), nugget = 0)
        vgm_model_ind <- suppressWarnings(
          fit.variogram(vgm_exp_ind, model = initial_ind,
                        fit.sills = TRUE, fit.ranges = TRUE, fit.kappa = TRUE)
        )
        kriged <- krige(indVar~1, df_ind, grid_df, model = vgm_model_ind)
      }
      # -----------------------------------------------------------
      
      # convert to data.frame & rename coords to x,y
      ddf <- as.data.frame(kriged)
      names(ddf)[1:2] <- c("x","y")
      
      # downstream: plot, download, map
      vals <- ddf$var1.pred
      pal  <- colorNumeric("viridis", domain = range(vals, na.rm = TRUE), na.color = "transparent")
      
      output$krigingPlot <- renderPlot({
        ggplot(ddf, aes(x=x, y=y, fill = var1.pred)) +
          geom_tile() + coord_equal() +
          scale_fill_viridis_c(name = "Prediction") +
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
        leaflet() %>% addTiles() %>%
          addRasterImage(r, colors = pal, opacity = 0.7) %>%
          addLegend(pal = pal, values = vals, title = "Kriging")
      })
      
      incProgress(1, "Done")
    })
  })
  
  # Cross-Validation is completely deactivated
})
