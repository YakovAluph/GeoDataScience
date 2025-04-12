# global.R
library(shiny)
library(shinythemes)
library(shinyjs)
library(gstat)
library(sp)
library(ggplot2)
library(leaflet)
library(raster)
library(data.table)
library(dplyr)

options(shiny.maxRequestSize = 30*1024^2)  # Increase max file upload size

# Global data cleaning/preparation function
cleanAndPrepareData <- function(data, xcol, ycol, valcol) {
  data <- data %>%
    select(x = !!sym(xcol), y = !!sym(ycol), value = !!sym(valcol)) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(x) & !is.na(y) & !is.na(value)) %>%
    filter(between(x, -180, 180) & between(y, -90, 90))
  
  if (nrow(data) < 3) stop("Not enough valid spatial data points to proceed.")
  
  coordinates(data) <- ~x + y
  proj4string(data) <- CRS("+proj=longlat +datum=WGS84")
  return(data)
}
