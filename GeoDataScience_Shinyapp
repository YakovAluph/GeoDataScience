# Load libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(gstat)
library(sp)
library(ggplot2)
library(leaflet)
library(raster)
library(data.table)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  titlePanel("Variogram, Kriging & Interactive Map"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("columnSelectors"),
      selectInput("coordSys", "Coordinate System:",
                  choices = c("Longitude / Latitude" = "longlat",
                              "Easting / Northing (UTM)" = "utm")),
      checkboxInput("useJitter", "Jitter Duplicates Instead of Averaging", FALSE),
      checkboxInput("quickPreview", "Quick Preview Mode (faster load)", FALSE),
      selectInput("modelType", "Choose Variogram Model:",
                  choices = c("Spherical" = "Sph",
                              "Exponential" = "Exp",
                              "Gaussian" = "Gau",
                              "Matern" = "Mat")),
      sliderInput("gridResolution", "Grid Resolution (number of points):", 
                  min = 10, max = 200, value = 50, step = 10),
      actionButton("plotBtn", "Run Analysis", class = "btn-primary"),
      actionButton("cvBtn", "Run Cross-Validation", class = "btn-info"),
      br(), br(),
      downloadButton("downloadKriging", "Download Kriging CSV"),
      downloadButton("downloadVariogram", "Download Variogram Plot"),
      downloadButton("downloadCV", "Download CV Results"),
      br(), br(),
      h5("Note: Ensure your dataset contains X, Y coordinates and a value column.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", tableOutput("dataPreview")),
        tabPanel("Variogram", plotOutput("variogramPlot"),
                 plotOutput("semiVariogramPlot")),
        tabPanel("Kriging", plotOutput("krigingPlot")),
        tabPanel("Map", leafletOutput("krigingMap", height = 600)),
        tabPanel("Cross-Validation", plotOutput("cvPlot"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  userData <- reactive({
    req(input$file)
    fread(input$file$datapath)
  })
  
  output$dataPreview <- renderTable({ userData() })
  
  output$columnSelectors <- renderUI({
    req(userData())
    cols <- names(userData())
    
    tagList(
      selectInput("xcol", "X Coordinate", choices = cols),
      selectInput("ycol", "Y Coordinate", choices = cols),
      selectInput("valcol", "Value Column", choices = cols)
    )
  })
  
  getCRS <- function() {
    if (input$coordSys == "utm") {
      return(CRS("+proj=utm +zone=33 +datum=WGS84"))  # change UTM zone as needed
    } else {
      return(CRS("+proj=longlat +datum=WGS84"))
    }
  }
  
  cleanAndPrepareData <- reactive({
    req(input$xcol, input$ycol, input$valcol)
    data <- userData()
    data[[input$xcol]] <- as.numeric(data[[input$xcol]])
    data[[input$ycol]] <- as.numeric(data[[input$ycol]])
    
    data <- data[!is.na(get(input$xcol)) & !is.na(get(input$ycol))]
    
    if (input$useJitter) {
      data[[input$xcol]] <- jitter(data[[input$xcol]], factor = 0.1)
      data[[input$ycol]] <- jitter(data[[input$ycol]], factor = 0.1)
      data <- data[, .(val = get(input$valcol)), 
                   by = .(x = get(input$xcol), y = get(input$ycol))]
    } else {
      data <- data[, .(val = mean(get(input$valcol), na.rm = TRUE)), 
                   by = .(x = get(input$xcol), y = get(input$ycol))]
    }
    
    if (nrow(data) == 0) return(NULL)
    
    coordinates(data) <- ~x + y
    proj4string(data) <- getCRS()
    return(data)
  })
  
  observeEvent(input$plotBtn, {
    withProgress(message = "Running analysis...", {
      data <- cleanAndPrepareData()
      req(data)
      
      formula_str <- as.formula("val ~ 1")
      vgm_exp <- variogram(formula_str, data)
      vgm_model <- fit.variogram(vgm_exp, model = vgm(input$modelType))
      
      output$variogramPlot <- renderPlot({
        plot(vgm_exp, model = vgm_model, main = paste("Variogram -", input$modelType))
      })
      
      output$semiVariogramPlot <- renderPlot({
        ggplot(vgm_exp, aes(x = dist, y = gamma)) +
          geom_point(color = "darkred", size = 2) +
          geom_line(color = "steelblue", linewidth = 1) +
          labs(title = paste("Semi-Variogram (", input$modelType, ")"),
               x = "Distance", y = "Semi-variance (γ)") +
          theme_minimal()
      })
      
      grid_res <- if (input$quickPreview) 25 else input$gridResolution
      bbox_vals <- bbox(data)
      x_range <- seq(bbox_vals[1,1], bbox_vals[1,2], length.out = grid_res)
      y_range <- seq(bbox_vals[2,1], bbox_vals[2,2], length.out = grid_res)
      grid <- expand.grid(x = x_range, y = y_range)
      coordinates(grid) <- ~x + y
      proj4string(grid) <- getCRS()
      
      kriged <- krige(formula_str, data, grid, model = vgm_model)
      
      output$krigingPlot <- renderPlot({
        kriged_df <- as.data.frame(kriged)
        ggplot(kriged_df, aes(x = x, y = y, fill = var1.pred)) +
          geom_tile() +
          coord_equal() +
          scale_fill_viridis_c(name = "Prediction") +
          labs(title = "Kriging Prediction", x = "X", y = "Y") +
          theme_minimal()
      })
      
      r <- rasterFromXYZ(as.data.frame(kriged)[, c("x", "y", "var1.pred")])
      crs(r) <- getCRS()
      
      output$krigingMap <- renderLeaflet({
        pal <- colorNumeric("viridis", values(r), na.color = "transparent")
        leaflet() %>%
          addTiles() %>%
          addRasterImage(r, colors = pal, opacity = 0.7) %>%
          addLegend(pal = pal, values = values(r), title = "Kriging")
      })
      
      output$downloadKriging <- downloadHandler(
        filename = function() paste0("kriging_", input$modelType, "_", Sys.Date(), ".csv"),
        content = function(file) write.csv(as.data.frame(kriged), file, row.names = FALSE)
      )
      
      output$downloadVariogram <- downloadHandler(
        filename = function() paste0("variogram_", input$modelType, "_", Sys.Date(), ".png"),
        content = function(file) {
          png(file, width = 800, height = 600)
          plot(vgm_exp, model = vgm_model)
          dev.off()
        }
      )
    })
  })
  
  # Cross-validation
  observeEvent(input$cvBtn, {
    withProgress(message = "Running cross-validation...", {
      data <- cleanAndPrepareData()
      req(data)
      
      formula_str <- as.formula("val ~ 1")
      vgm_exp <- variogram(formula_str, data)
      vgm_model <- fit.variogram(vgm_exp, model = vgm(input$modelType))
      
      cv_result <- krige.cv(formula_str, data, model = vgm_model, nfold = 10)
      cv_df <- as.data.frame(cv_result)
      
      output$cvPlot <- renderPlot({
        ggplot(cv_df, aes(x = observed, y = var1.pred)) +
          geom_point(color = "darkblue", alpha = 0.7) +
          geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
          labs(title = "Cross-Validation: Observed vs Predicted",
               x = "Observed", y = "Predicted") +
          theme_minimal()
      })
      
      output$downloadCV <- downloadHandler(
        filename = function() paste0("cross_validation_", Sys.Date(), ".csv"),
        content = function(file) write.csv(cv_df, file, row.names = FALSE)
      )
    })
  })
}

# Run app
shinyApp(ui = ui, server = server)
