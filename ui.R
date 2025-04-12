library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  titlePanel("Variogram, Kriging & Interactive Map"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
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
      actionButton("plotBtn", "Run Analysis", class = "btn-primary"),
      actionButton("cvBtn", "Run Cross-Validation", class = "btn-info"),
      br(), br(),
      h5("Note: Ensure that your dataset contains X, Y coordinates and a value column.")
    ),
    
    mainPanel(
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
))
