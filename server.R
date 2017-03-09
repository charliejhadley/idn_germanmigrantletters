library(rgdal)
library(GISTools)
library(shiny)
library(leaflet)
library(tidyverse)
library(sp)
library(statesRcontiguous)

library(geosphere) # for great circles
library(shinyjs)

source(file = "data-processing.R", local = TRUE)

source(file = "GIS.R", local = TRUE)

shinyServer(
  function(input, output){
    
    # runcodeServer() # for shinyjs::runcodeUI()
    
    source("us_choropleth.R", local = TRUE)$value
    
    source("letter_journeys.R", local = TRUE)$value
    
  }
)