library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  tags$head(tags$style(HTML("
    .btn-custom {
      background-color: #18536f; color: white; border-color: #18536f;
    }
    .btn-custom:hover {
      background-color: #143f52; border-color: #143f52;
    }
    .download-button .btn {
      background-color: #18536f; color: white; border-color: #18536f;
    }
    .download-button .btn:hover {
      background-color: #143f52; border-color: #143f52;
    }
    .tab-disabled .nav-link {
      color: gray !important; pointer-events: none;
    }
    .tab-enabled .nav-link {
      color: #18536f !important; font-weight: bold;
    }
  "))),
  
  titlePanel("GeoDataScience: An interactive tool for Variogram and Kriging"),
  
  sidebarLayout(
    sidebarPanel(
      tags$b("Upload your data (.csv, .txt, .xls, or .xlsx)"),
      
      # File input + “please upload” message
      tags$div(
        fileInput("file", NULL,
                  accept = c(".csv", ".txt", ".xls", ".xlsx")),
        uiOutput("fileRequiredMsg")
      ),
      
      # X/Y same-column error
      uiOutput("dataWarning"),
      
      # Instruction
      h5("Note: Ensure that your dataset contains X, Y coordinates and a value column."),
      
      # Column selectors
      uiOutput("columnSelectors"),
      
      # Coordinate type as radios
      radioButtons("coordType", "Coordinate Type:",
                   choices = c(
                     "Latitude/Longitude"        = "longlat",
                     "Easting/Northing (UTM etc)"= "projected"
                   ),
                   selected = "longlat", inline = TRUE),
      
      # Variogram model as radios
      radioButtons("modelType", "Choose Variogram Model:",
                   choices = c(
                     "Spherical"   = "Sph",
                     "Exponential" = "Exp",
                     "Gaussian"    = "Gau",
                     "Matern"      = "Mat"
                   ),
                   selected = "Sph", inline = TRUE),
      
      # Grid resolution
      sliderInput("gridResolution", "Grid Resolution:", 
                  min = 10, max = 200, value = 100, step = 10),
      
      # Quick preview toggle
      checkboxInput("quickPreview", "Quick Preview (Variogram only)", FALSE),
      
      # Run & Reset side by side
      tags$div(style="display:flex; gap:8px; margin-bottom:10px;",
               actionButton("plotBtn",  "Run Analysis", class = "btn btn-custom"),
               actionButton("resetBtn", "Reset Values", class = "btn btn-custom")
      ),
      
      # CV prompt
      uiOutput("cvBox")
    ),
    
    mainPanel(
      uiOutput("mainTabs")
    )
  )
))
