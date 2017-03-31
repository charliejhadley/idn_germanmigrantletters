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

source(file = "pagerui.R")

## ======================== custom legend
## ================================================
## from http://stackoverflow.com/a/37482936/1659890

addLegendCustom <-
  function(map, colors, labels, sizes, opacity = 0.5) {
    colorAdditions <-
      paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <-
      paste0(
        "<div style='display: inline-block;height: ",
        sizes,
        "px;margin-top: 4px;line-height: ",
        sizes,
        "px;'>",
        labels,
        "</div>"
      )
    
    return(addLegend(
      map,
      colors = colorAdditions,
      labels = labelAdditions,
      opacity = opacity
    ))
  }


shinyServer(
  function(input, output, session){

    
    # runcodeServer() # for shinyjs::runcodeUI()
    
    source("us_choropleth.R", local = TRUE)$value
    
    source("letter_journeys.R", local = TRUE)$value
    
    source("selected-family.R", local = TRUE)$value
    
  }
)