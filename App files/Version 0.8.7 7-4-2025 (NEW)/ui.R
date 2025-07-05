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
        
        tags$b("Upload your data (.csv, .txt, .xls, or .xlsx)"),
        tags$div(
          fileInput("file", NULL,
                    accept = c(".csv", ".txt", ".xls", ".xlsx")),
          div(tags$b("Drag and drop file"), style = "text-align: center; margin-top: -10px; margin-bottom: 10px;"),
          uiOutput("fileRequiredMsg")
        ),
        uiOutput("dataWarning"),
        h5("Note: Ensure that your dataset contains X, Y coordinates and a value column."),
        uiOutput("columnSelectors"),
        
        div(style = "margin-bottom: 15px;",
            radioButtons("coordType", "Coordinate Type",
                         choices = c(
                           "Latitude/Longitude"         = "longlat",
                           "Easting/Northing (UTM etc)" = "projected"
                         ),
                         selected = "longlat",
                         inline = FALSE)
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
        
        sliderInput("gridResolution", "Grid Resolution",
                    min = 10, max = 200, value = 50, step = 10),
        checkboxInput("quickPreview", "Quick Preview (Only Variogram)", FALSE),
        
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
        
        # 🔥 Directional column selector for Rose Diagram
        selectizeInput("dircol", "Directional Column (for Rose Diagram)", 
                       choices = NULL, 
                       selected = NULL, 
                       multiple = FALSE),
        
        tags$div(style = "display: flex; gap: 8px; margin-bottom: 10px;",
                 actionButton("plotBtn",  "Run Analysis", class = "btn btn-custom"),
                 actionButton("resetBtn", "Reset Values",  class = "btn btn-custom")
        ),
        
        uiOutput("cvBox")
      )
    ),
    
    mainPanel(
      uiOutput("mainTabs")
    )
  )
))
