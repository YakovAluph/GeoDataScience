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
              selectInput("coordinateScale", "Coordinate units in the uploaded table",
                choices = c("Already in the CRS units" = "1", "Kilometres; convert to metres" = "1000"), selected = "1"),
              helpText("Select the CRS already used by your X/Y coordinates. Choose it from the dataset metadata; location alone does not identify the datum.")
            ),
            uiOutput("sourceCRSInfo")
        ),
        
        div(style = "margin-bottom: 15px;",
            selectInput("dataTransform", "Measurement transformation",
              choices = c("Raw values" = "raw", "log(1 + value), non-negative data" = "log1p"), selected = "raw"),
            helpText("Log predictions are back-transformed medians; uncertainty stays on the log scale. No zero values are replaced automatically."),
            selectInput("modelType", "Variogram model",
                         choices = c(
                           "Compare models automatically" = "Auto",
                           "Spherical"   = "Sph",
                           "Exponential" = "Exp",
                           "Gaussian"    = "Gau",
                           "Matern (smoothness search)" = "Mat",
                           "Stein Matern (smoothness search)" = "Ste",
                           "Nugget baseline" = "Nug"
                         ),
                         selected = "Auto"),
            selectInput("selectionCriterion", "Automatic model selection",
              c("Cross-validation RMSE" = "CV_RMSE", "Weighted variogram fit error" = "SSE"), selected = "CV_RMSE")
        ),
        
        div(style = "margin-bottom: 10px;",
            tags$label("Grid Resolution"),
            sliderInput("gridResolution", NULL, min = 10, max = 200, value = 50, step = 10),
            div(style = "font-size: 12px; color: gray; margin-top: -10px;",
                "Higher kriging resolution = longer rendering process (Set to 50 for medium speed)")
        ),
        
        checkboxInput("quickPreview", "Quick Preview (skip prediction grid)", FALSE),
        tags$details(
          tags$summary("Model fitting and validation settings"),
          numericInput("nmax", "Maximum neighbours per prediction", value = 40, min = 5, max = 500, step = 5),
          numericInput("fitMax", "Maximum locations for empirical variograms", value = 1500, min = 50, max = 5000, step = 100),
          sliderInput("lagBins", "Variogram lag bins", min = 5, max = 40, value = 15),
          sliderInput("cutoffFraction", "Cutoff fraction of initial empirical distance span", min = 0.25, max = 1, value = 1, step = 0.05),
          checkboxInput("runCV", "Run cross-validation", TRUE),
          conditionalPanel("input.runCV",
            numericInput("cvFolds", "Validation folds", value = 5, min = 2, max = 10, step = 1),
            selectInput("cvType", "Validation split", c("Random folds" = "random", "Spatial cluster folds" = "spatial")),
            numericInput("cvMax", "Maximum held-out locations scored", value = 500, min = 20, max = 5000, step = 100),
            numericInput("cvSeed", "Reproducible random seed", value = 42, min = 1, max = 1000000)),
          selectInput("predictionMask", "Prediction area",
            c("Sample convex hull" = "hull", "Full bounding rectangle" = "none", "Within distance of a sample" = "distance")),
          conditionalPanel("input.predictionMask === 'distance'",
            numericInput("maxMaskDistance", "Maximum sample distance (metres)", value = 1000, min = 1)),
          helpText("Variograms may use a reproducible subset. Final predictions use all valid locations with the selected local neighbourhood. CV refits each model in each training fold.")
        ),
        
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
        
        # The secondary selector is rendered once in columnSelectors.
        
        tags$div(style = "display: flex; gap: 8px; margin-bottom: 10px;",
                 actionButton("plotBtn",  "Run Analysis", class = "btn btn-custom"),
                 actionButton("resetBtn", "Reset Values",  class = "btn btn-custom")
        ),
        
        helpText("Histogram, density and count plots are under Data exploration. Validation diagnostics have their own tab.")
      )
    ),
    
    mainPanel(
      uiOutput("mainTabs")
    )
  )
))
