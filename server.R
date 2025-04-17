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
  
  dataReady <- reactiveVal(FALSE)
  errorMsg  <- reactiveVal(NULL)
  
  # Reset: only clear flags and inputs
  observeEvent(input$resetBtn, {
    dataReady(FALSE)
    errorMsg(NULL)
    reset("file")
  })
  
  # Read user data
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
      return(NULL)
    })
  })
  
  # Preview
  output$dataPreview <- renderTable({
    req(userData())
    head(userData(), 10)
  })
  
  # Column selectors
  output$columnSelectors <- renderUI({
    req(userData())
    cols <- names(userData())
    tagList(
      selectInput("xcol", "X Coordinate",   choices = cols),
      selectInput("ycol", "Y Coordinate",   choices = cols),
      selectInput("valcol","Value Column",  choices = cols),
      checkboxInput("quickPreview",
                    "Quick Preview (Variogram only)",
                    value = FALSE)
    )
  })
  
  # Tabs UI (reactive based on dataReady())
  output$mainTabs <- renderUI({
    if (!dataReady()) {
      div(class = "tab-disabled",
          tabsetPanel(
            tabPanel("Data",             h5("Upload a valid dataset to begin.")),
            tabPanel("Variogram",        NULL),
            tabPanel("Kriging",          NULL),
            tabPanel("Map",              NULL),
            tabPanel("Cross-Validation", NULL)
          )
      )
    } else {
      div(class = "tab-enabled",
          tabsetPanel(
            tabPanel("Data", tableOutput("dataPreview")),
            tabPanel("Variogram",
                     plotOutput("variogramPlot"),
                     plotOutput("semiVariogramPlot"),
                     div(class="download-button",
                         downloadButton("downloadVariogram",
                                        "Download Variogram Plot"))
            ),
            tabPanel("Kriging",
                     conditionalPanel("!input.quickPreview",
                                      plotOutput("krigingPlot"),
                                      div(class="download-button",
                                          downloadButton("downloadKriging",
                                                         "Download Kriging CSV"))
                     )
            ),
            tabPanel("Map",
                     conditionalPanel("!input.quickPreview",
                                      leafletOutput("krigingMap", height = 600)
                     )
            ),
            tabPanel("Cross-Validation",
                     conditionalPanel("!input.quickPreview",
                                      plotOutput("cvPlot"),
                                      div(class="download-button",
                                          downloadButton("downloadCV",
                                                         "Download CV Results"))
                     )
            )
          )
      )
    }
  })
  
  # Show any error messages
  output$dataWarning <- renderUI({
    req(errorMsg())
    div(style="color:red;font-weight:bold;", errorMsg())
  })
  
  # Prepare spatial data
  cleanAndPrepareData <- reactive({
    req(userData(), input$xcol, input$ycol, input$valcol)
    
    if (input$xcol == input$ycol) {
      errorMsg("X and Y columns cannot be the same.")
      dataReady(FALSE)
      return(NULL)
    }
    
    data <- userData()
    data[[input$xcol]] <- as.numeric(data[[input$xcol]])
    data[[input$ycol]] <- as.numeric(data[[input$ycol]])
    data[[input$valcol]] <- as.numeric(data[[input$valcol]])
    data <- na.omit(data[, c(input$xcol, input$ycol, input$valcol),
                         with = FALSE])
    
    data <- data %>%
      group_by_at(c(input$xcol, input$ycol)) %>%
      summarise(across(all_of(input$valcol), mean),
                .groups = "drop")
    
    set.seed(42)
    data[[input$xcol]] <- jitter(data[[input$xcol]], factor = 0.0001)
    data[[input$ycol]] <- jitter(data[[input$ycol]], factor = 0.0001)
    coordinates(data) <- c(input$xcol, input$ycol)
    
    # Validate CRS ranges
    xvals <- coordinates(data)[,1]
    yvals <- coordinates(data)[,2]
    
    if (input$coordType == "longlat") {
      if ( any(xvals < -180 | xvals > 180, na.rm=TRUE) ||
           any(yvals < -90  | yvals >  90, na.rm=TRUE) ) {
        showModal(modalDialog(
          title = "Invalid Coordinates",
          "Those columns are not valid long/lat.  
           Please upload data with real geographic coordinates.",
          easyClose = TRUE
        ))
        dataReady(FALSE)
        return(NULL)
      }
      proj4string(data) <- CRS("+proj=longlat +datum=WGS84")
    } else {
      if ( any(!is.finite(xvals)) || any(!is.finite(yvals)) ) {
        showModal(modalDialog(
          title = "Invalid Coordinates",
          "X/Y must be numeric projected coords (e.g. UTM).",
          easyClose = TRUE
        ))
        dataReady(FALSE)
        return(NULL)
      }
      proj4string(data) <- CRS("+proj=utm +zone=33 +datum=WGS84")
    }
    
    errorMsg(NULL)
    dataReady(TRUE)
    return(data)
  })
  
  # Run Analysis
  observeEvent(input$plotBtn, {
    errorMsg(NULL)
    data <- cleanAndPrepareData()
    req(data)
    
    withProgress(message='Running Analysis...', value=0.1, {
      formula_str <- as.formula(paste(input$valcol,"~ 1"))
      vgm_exp   <- variogram(formula_str, data)
      vgm_model <- fit.variogram(vgm_exp,
                                 model=vgm(input$modelType))
      
      incProgress(0.3, "Fitting Variogram")
      output$variogramPlot <- renderPlot({
        plot(vgm_exp, model=vgm_model,
             main=paste("Variogram -", input$modelType))
      })
      output$semiVariogramPlot <- renderPlot({
        ggplot(vgm_exp, aes(x=dist,y=gamma))+
          geom_point(color="darkred",size=2)+
          geom_line(color="steelblue",linewidth=1)+
          labs(title=paste("Semi-Variogram (",input$modelType,")"),
               x="Distance",y="Semi-variance (γ)")+
          theme_minimal()
      })
      output$downloadVariogram <- downloadHandler(
        filename=function() paste0("variogram_plot_",
                                   input$modelType,"_",Sys.Date(),".png"),
        content=function(file){
          png(file,width=800,height=600)
          plot(vgm_exp, model=vgm_model,
               main=paste("Variogram -",input$modelType))
          dev.off()
        }
      )
      
      if (!input$quickPreview) {
        incProgress(0.6,"Running Kriging")
        grid_res  <- input$gridResolution
        b         <- bbox(data)
        x_range   <- seq(b[1,1],b[1,2],length.out=grid_res)
        y_range   <- seq(b[2,1],b[2,2],length.out=grid_res)
        grid      <- expand.grid(x=x_range,y=y_range)
        coordinates(grid) <- ~x+y
        proj4string(grid) <- proj4string(data)
        kriged <- krige(formula_str,data,grid,model=vgm_model)
        
        output$krigingPlot <- renderPlot({
          ddf <- as.data.frame(kriged)
          ggplot(ddf,aes(x=x,y=y,fill=var1.pred))+
            geom_tile()+coord_equal()+
            scale_fill_viridis_c(name="Prediction")+
            labs(title="Kriging Prediction",x="X",y="Y")+
            theme_minimal()
        })
        output$downloadKriging <- downloadHandler(
          filename=function()paste0("kriging_results_",
                                    input$modelType,"_",Sys.Date(),".csv"),
          content=function(file){
            write.csv(as.data.frame(kriged),file,row.names=FALSE)
          }
        )
        
        r <- rasterFromXYZ(as.data.frame(kriged)[,c("x","y","var1.pred")])
        crs(r) <- CRS(proj4string(data))
        output$krigingMap <- renderLeaflet({
          pal <- colorNumeric("viridis",values(r),na.color="transparent")
          leaflet()%>%addTiles()%>%
            addRasterImage(r,colors=pal,opacity=0.7)%>%
            addLegend(pal=pal,values=values(r),title="Kriging")
        })
      }
      incProgress(1,"Done")
    })
  })
  
  # Cross-validation
  observeEvent(input$cvBtn,{
    req(dataReady())
    data <- cleanAndPrepareData()
    
    withProgress(message='Cross-validating...',value=0,{
      n          <- nrow(data)
      pred       <- numeric(n)
      obs        <- numeric(n)
      formula_str<- as.formula(paste(input$valcol,"~ 1"))
      vgm_model  <- fit.variogram(
        variogram(formula_str,data),
        model=vgm(input$modelType)
      )
      
      for(i in 1:n){
        incProgress(1/n,detail=paste("Point",i,"of",n))
        train_data <- data[-i,]
        test_data  <- data[i,,drop=FALSE]
        res        <- tryCatch({
          krige(formula_str,train_data,
                test_data,model=vgm_model)
        },error=function(e)NULL)
        pred[i] <- if(!is.null(res))res$var1.pred else NA
        obs [i] <- test_data[[input$valcol]]
      }
      
      output$cvPlot <- renderPlot({
        df <- data.frame(Observed=obs,Predicted=pred)
        ggplot(df,aes(x=Observed,y=Predicted))+
          geom_point(color="blue")+
          geom_abline(slope=1,intercept=0,
                      color="red",linetype="dashed")+
          labs(title="Cross-Validation: Observed vs Predicted")+
          theme_minimal()
      })
      output$downloadCV <- downloadHandler(
        filename=function()paste0("cv_results_",Sys.Date(),".csv"),
        content=function(file){
          write.csv(data.frame(Observed=obs,Predicted=pred),
                    file,row.names=FALSE)
        }
      )
    })
  })
  
  # Display error messages
  output$errorMessage <- renderUI({
    req(errorMsg())
    div(style="color:red;font-weight:bold;padding:10px;",
        errorMsg())
  })
})
