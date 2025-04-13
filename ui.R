library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyFeedback)  # NEW

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  useShinyFeedback(),  # Enable feedback
  
  tags$head(
    tags$style(HTML("
      .btn-primary { background-color: #1b6ec2; border-color: #1b6ec2; }
      .btn-info { background-color: #17a2b8; }
      .btn-danger { background-color: #dc3545; }
      h5, h4, h3 { color: #2c3e50; }
    "))
  ),
  
  titlePanel("🌍 GeoDataScience: Interactive Variogram & Kriging Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", strong("Upload your data"),
                accept = c(".csv", ".txt", ".xls", ".xlsx")),
      
      uiOutput("columnSelectors"),
      
      selectInput("coordType", 
                  label = "Coordinate Type:",
                  choices = c("Latitude/Longitude" = "longlat",
                              "Easting/Northing (UTM or other)" = "projected")),
      tags$small("Tip: Choose projected for UTM or similar coordinates."),
      
      selectInput("modelType", "Choose Variogram Model:",
                  choices = c("Spherical" = "Sph",
                              "Exponential" = "Exp",
                              "Gaussian" = "Gau",
                              "Matern" = "Mat")),
      tags$small("These influence how spatial continuity is modeled."),
      
      sliderInput("gridResolution", 
                  "Grid Resolution:",
                  min = 10, max = 200, value = 50, step = 10),
      tags$small("Higher values = finer resolution (slower performance)"),
      
      checkboxInput("quickPreview", 
                    "Quick Preview (Skip Kriging/Map)", value = FALSE),
      
      actionButton("plotBtn", "Run Analysis", class = "btn-primary"),
      actionButton("cvBtn", "Run Cross-Validation", class = "btn-info"),
      actionButton("resetBtn", "Reset Values", class = "btn-danger"),
      
      br(), br(),
      h5("Note: Your file must include X/Y coordinates and a value column.")
    ),
    
    mainPanel(
      shinyjs::hidden(
        div(id = "resultsPanel",
            tabsetPanel(
              tabPanel("Data", tableOutput("dataPreview")),
              tabPanel("Variogram",
                       plotOutput("variogramPlot"),
                       plotOutput("semiVariogramPlot"),
                       downloadButton("downloadVariogram", "Download Variogram Plot")),
              tabPanel("Kriging",
                       plotOutput("krigingPlot"),
                       downloadButton("downloadKriging", "Download Kriging CSV")),
              tabPanel("Map", leafletOutput("krigingMap", height = 600)),
              tabPanel("Cross-Validation",
                       plotOutput("cvPlot"),
                       downloadButton("downloadCV", "Download CV Results"))
            )
        )
      )
    )
  )
))
