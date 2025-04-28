library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  # Custom CSS styling
  tags$head(
    tags$style(HTML("
      .btn-custom {
        background-color: #18536f;
        color: white;
        border-color: #18536f;
      }
      .btn-custom:hover {
        background-color: #143f52;
        border-color: #143f52;
      }
      .download-button .btn {
        background-color: #18536f;
        color: white;
        border-color: #18536f;
      }
      .download-button .btn:hover {
        background-color: #143f52;
        border-color: #143f52;
      }
      .tab-disabled .nav-link {
        color: gray !important;
        pointer-events: none;
      }
      .tab-enabled .nav-link {
        color: #18536f !important;
        font-weight: bold;
      }
    "))
  ),
  
  # Title
  titlePanel("GeoDataScience: An interactive tool for Variogram and Kriging"),
  
  sidebarLayout(
    sidebarPanel(
      tags$b("Upload your data (.csv, .txt, .xls, or .xlsx)"),
      
      # File upload + red warning if missing
      tags$div(
        fileInput("file", NULL,
                  accept = c(".csv", ".txt", ".xls", ".xlsx")),
        uiOutput("fileRequiredMsg")
      ),
      
      # Validation error (e.g. X == Y)
      uiOutput("dataWarning"),
      
      # Instructional note
      h5("Note: Ensure that your dataset contains X, Y coordinates and a value column."),
      
      # Column selectors
      uiOutput("columnSelectors"),
      
      # Coordinate type as radio buttons
      radioButtons("coordType", "Coordinate Type",
                   choices = c(
                     "Latitude/Longitude"         = "longlat",
                     "Easting/Northing (UTM etc)" = "projected"
                   ),
                   selected = "longlat", inline = TRUE),
      
      # Variogram model function as radio buttons
      radioButtons("modelType", "Variogram Model Function",
                   choices = c(
                     "Spherical"   = "Sph",
                     "Exponential" = "Exp",
                     "Gaussian"    = "Gau",
                     "Matern"      = "Mat"
                   ),
                   selected = "Sph", inline = TRUE),
      
      # Grid resolution slider
      sliderInput("gridResolution", "Grid Resolution",
                  min = 10, max = 200, value = 50, step = 10),
      
      # Quick preview toggle
      checkboxInput("quickPreview", "Quick Preview (Only Variogram)", value = FALSE),
      
      # Run & Reset buttons side by side
      tags$div(style = "display: flex; gap: 8px; margin-bottom: 10px;",
               actionButton("plotBtn",  "Run Analysis", class = "btn btn-custom"),
               actionButton("resetBtn", "Reset Values", class = "btn btn-custom")
      ),
      
      # Cross-validation prompt (appears after analysis)
      uiOutput("cvBox")
    ),
    
    mainPanel(
      uiOutput("mainTabs")
    )
  )
))
