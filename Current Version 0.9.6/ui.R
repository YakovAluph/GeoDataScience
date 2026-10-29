## ui.R ##
library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  tags$head(tags$style(HTML("\
    /* Make all buttons bold */
    .btn, .btn-custom, .download-button .btn {
      font-weight: bold !important;
    }

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

    /* Drag and drop styling */
    .shiny-file-input {
      border: 2px dashed #ccc;
      padding: 10px;
      background-color: #f9f9f9;
      text-align: center;
    }
    .shiny-file-input:hover {
      border-color: #18536f;
    }
  "))),
  
  titlePanel("GeoDataScience: An interactive tool for Variogram and Kriging"),
  
  sidebarLayout(
    sidebarPanel(
      tags$form(
        id = "appForm",
        
        tags$b("Upload CSV, TXT, Excel, ASCII, TIFF, XML or GeoJSON data"),
        tags$div(
          uiOutput("fileInputUI"),
          div(tags$b("Drag and drop file"), style = "text-align: center; margin-top: -10px; margin-bottom: 10px;"),
          uiOutput("fileRequiredMsg")
        ),
        uiOutput("rasterCRSUI"),
        uiOutput("dataWarning"),
        h5("Select X, Y and a numeric measurement. Raster cells and GeoJSON points provide X/Y automatically."),
        helpText("ASCII: .asc/.ascii grids or delimited tables. TIFF: georeferenced rasters. GeoJSON: points with measurement properties."),
        helpText("XML: flat <row>, <record> or <point> elements with coordinate and measurement fields."),
        uiOutput("columnSelectors"),
        
        div(style = "margin-bottom: 15px;",
            radioButtons("coordType", "Coordinate Type",
                         choices = c(
                           "Longitude/Latitude"         = "longlat",
                           "Easting/Northing (UTM, ITM or other)" = "projected"
                         ),
                         selected = "longlat",
                         inline = FALSE),
            conditionalPanel(
              condition = "input.coordType === 'projected'",
              projectionControls("source"),
              helpText("Select the CRS already used by your X/Y coordinates. Choose it from the dataset metadata; location alone does not identify the datum.")
            ),
            uiOutput("sourceCRSInfo")
        ),
        
        div(style = "margin-bottom: 15px;",
            radioButtons("modelType", "Variogram Model Function",
                         choices = c(
                           "Spherical"   = "Sph",
                           "Exponential" = "Exp",
                           "Gaussian"    = "Gau",
                           "Matern"      = "Mat"
                         ),
                         selected = "Sph",
                         inline = FALSE)
        ),
        
        div(style = "margin-bottom: 10px;",
            tags$label("Grid Resolution"),
            sliderInput("gridResolution", NULL, min = 10, max = 200, value = 50, step = 10),
            div(style = "font-size: 12px; color: gray; margin-top: -10px;",
                "Higher kriging resolution = longer rendering process (Set to 50 for medium speed)")
        ),
        
        checkboxInput("quickPreview", "Quick Preview (Variogram Only)", FALSE),
        
        div(style = "margin-bottom: 15px;",
            radioButtons("krigingType", "Kriging Method",
                         choices = c(
                           "Simple Kriging"    = "SK",
                           "Ordinary Kriging"  = "OK",
                           "Universal Kriging" = "UK",
                           "Indicator Kriging" = "IK",
                           "Co-Kriging"         = "CK"
                         ),
                         selected = "OK",
                         inline = FALSE)
        ),
        
        conditionalPanel(
          condition = "input.krigingType == 'IK'",
          numericInput("ikThreshold", "Indicator Threshold", value = 0)
        ),
        
        conditionalPanel(
          condition = "input.krigingType == 'CK'",
          selectizeInput("secondaryVar", "Secondary Variable (Covariate)", choices = NULL)
        ),
        
        tags$div(style = "display: flex; gap: 8px; margin-bottom: 10px;",
                 actionButton("plotBtn",  "Run Analysis", class = "btn btn-custom"),
                 actionButton("resetBtn", "Reset Values",  class = "btn btn-custom"),
                 hidden(actionButton("stopBtn", "Stop Analysis", class = "btn btn-danger"))
        ),
        
        uiOutput("cvBox")
      )
    ),
    
    mainPanel(
      uiOutput("mainTabs")
    )
  )
))
