library("rgdal")
library("GISTools")
library("shiny")
library("leaflet")
library("tidyverse")
library("readxl")
library("sp")
library("statesRcontiguous")
library("lubridate")
library("shinyjs")
library("stringr")
library("rlang")
library("sf")
library("forcats")
library("rfigshare")
library("geosphere")

source(file = "data-processing.R", local = TRUE)

source(file = "GIS.R", local = TRUE)

source(file = "pagerui.R")

## ======================== custom legend
## ================================================
## from http://stackoverflow.com/a/37482936/1659890


shinyServer(
  function(input, output, session){

    
    # runcodeServer() # for shinyjs::runcodeUI()
    
    source("tab_choropleth.R", local = TRUE)$value
    
    source("tab_letter_journeys.R", local = TRUE)$value
    
    source("selected-family.R", local = TRUE)$value
    
    source("tab_selected-family-letters.R", local = TRUE)$value
  }
)