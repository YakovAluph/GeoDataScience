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
      .btn-reset {
        background-color: #8B0000;
        color: white;
        border-color: #8B0000;
      }
      .btn-reset:hover {
        background-color: #6b0000;
        border-color: #6b0000;
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
      fileInput("file", NULL,
                accept = c(".csv", ".txt", ".xls", ".xlsx")),
      
      uiOutput("columnSelectors"),
      
      selectInput("coordType", "Coordinate Type:",
                  choices = c("Latitude/Longitude" = "longlat",
                              "Easting/Northing (UTM or other)" = "projected")),
      selectInput("modelType", "Choose Variogram Model:",
                  choices = c("Spherical" = "Sph",
                              "Exponential" = "Exp",
                              "Gaussian" = "Gau",
                              "Matern" = "Mat")),
      sliderInput("gridResolution", "Grid Resolution (number of points):",
                  min = 10, max = 200, value = 50, step = 10),
      
      checkboxInput("quickPreview", "Quick Preview (Only Variogram)", value = FALSE),
      
      actionButton("plotBtn", "Run Analysis", class = "btn btn-custom"),
      
      uiOutput("cvBox"),           # ← dynamically-rendered CV prompt/button
      
      actionButton("resetBtn", "Reset Values", class = "btn btn-reset"),
      
      br(), br(),
      h5("Note: Ensure that your dataset contains X, Y coordinates and a value column.")
    ),
    
    mainPanel(
      uiOutput("dataWarning"),     # for showing warnings like "X and Y cannot be same"
      
      uiOutput("mainTabs")         # dynamic rendering of the tabsetPanel
    )
  )
))
