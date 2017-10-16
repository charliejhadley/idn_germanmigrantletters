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

source(file = "data-processing.R", local = TRUE)

source(file = "GIS.R", local = TRUE)

## ================================= Choropleth ======================================
## ===================================================================================

## ========== Utility functions
## ==============================

choropleth_legend_title <- function(start.year,
                         end.year,
                         how.tally,
                         cumulative){
  
  how_tallied <- list("sender" = "Letters Sent",
                      "receiver" = "Letters Received",
                      "both" = "Letters Sent & Received")
  
  if(cumulative == TRUE){
    paste0(how_tallied[[how.tally]], "<br>",
           "Before ", end.year)
  } else {
    paste0(how_tallied[[how.tally]], "<br>",
           start.year, " - ", end.year)
  }
  
}

base_choropleth_leaflet <- function(data,
                                    start.year,
                                    end.year,
                                    how.tally,
                                    cumulative) {
  the_fillColor <- switch(
    how.tally,
    "sender" = "send.count",
    "receiver" = "received.count",
    "both" = "total.count"
  )
  
  the_title_legend <- switch(
    how.tally,
    "sender" = "Letters Sent",
    "receiver" = "Letters Received",
    "both" = "Letters Sent & Received"
  )
  
  us_palette <- colorBin(
    c("#cccccc", brewer.pal(5, "YlGnBu")),
    bins = c(0, 1, 5, 10, 20, 50, 350),
    pretty = FALSE
  )
  
  bounds <-
    c(-120, 26 , -70, 47) # http://rpubs.com/bhaskarvk/proj4leaflet

  leaflet(data = data,
          options = leafletOptions(zoomControl = FALSE,
                                   minZoom = 2.5)) %>%
    addPolygons(
      data = data,
      fillColor = as.formula(paste0("~us_palette(", the_fillColor, ")")),
      stroke = TRUE,
      color = "#737373",
      smoothFactor = 0.2,
      fillOpacity = 0.8,
      weight = 1,
      popup = ~ paste0(
        state.name,
        "</br>",
        "Letter sent: ",
        send.count,
        "</br>",
        "Letter received: ",
        received.count
      )
    ) %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addLegend(pal = us_palette,
              values = as.formula(paste0("~", the_fillColor)),
              position = "bottomright",
              title = choropleth_legend_title(start.year,
                                              end.year,
                                              how.tally = how.tally,
                                              cumulative = cumulative))
}

make_img_choropleth <- function(start.year,
                                end.year,
                                shapefile = "states",
                                how.tally = "sender",
                                cumulative = FALSE) {
  selected_shapefile <- switch(
    shapefile,
    "states" = states_shapefiles,
    "counties" = counties_shapefiles,
    "congressional districts" = congressional_districts_shapefiles
  )
  
  choropleth_filtered_letters <- letters_df %>%
    filter(!is.na(sender.latitude)) # sender latitude must exist for this visualisation
  
  if(cumulative == TRUE){
    choropleth_filtered_letters <- choropleth_filtered_letters %>%
      filter(!is.na(date)) %>%
      filter(date <= dmy(paste0("31-12-", end.year)))
    
  } else{
    choropleth_filtered_letters <- choropleth_filtered_letters %>%
      filter(!is.na(date)) %>%
      filter(date >= dmy(paste0("01-01-", start.year)) &
               date <= dmy(paste0("31-12-", end.year)))
    
  }
  
  choropleth_sf_tally <-
    count_letters_in_shp(choropleth_filtered_letters, selected_shapefile)

  bounds <-
    c(-130, 26 ,-70, 47) # http://rpubs.com/bhaskarvk/proj4leaflet
  
  switch(
    shapefile,
    "states" = {
      base_choropleth_leaflet(choropleth_sf_tally,
                              start.year,
                              end.year,
                              how.tally,
                              cumulative)
    },
    "congressional districts" = {
      base_choropleth_leaflet(choropleth_sf_tally,
                              start.year,
                              end.year,
                              how.tally,
                              cumulative) %>%
        addPolygons(
          data = states_shapefiles,
          color = "#737373",
          weight = 1,
          opacity = 1,
          fillOpacity = 0,
          options = pathOptions(clickable = FALSE)
        )
    },
    "counties" = {
      base_choropleth_leaflet(choropleth_sf_tally,
                              start.year,
                              end.year,
                              how.tally,
                              cumulative) %>%
        addPolygons(
          data = states_shapefiles,
          color = "#737373",
          weight = 1,
          opacity = 1,
          fillOpacity = 0,
          options = pathOptions(clickable = FALSE)
        )
    }
  )
  
}

## ========== Single example
## ==============================

make_img_choropleth(
  start.year = 1850,
  end.year = 1860,
  how.tally = "sender",
  shapefile = "states"
)

the_choropleth <- make_img_choropleth(
  start.year = 1850,
  end.year = 1860,
  how.tally = "sender",
  shapefile = "congressional districts"
)

tmp_fp <- "gif-store/choropleth.html"

tmp_fp <- paste0("gif-store/", 1810, "-", 1810 + 10, ".html")
htmlwidgets::saveWidget(
  widget = the_choropleth,
  file = file.path(normalizePath(dirname(tmp_fp)), basename(tmp_fp)),
  selfcontained = FALSE
)
webshot::webshot(url = tmp_fp,
                 file = paste0(tools::file_path_sans_ext(tmp_fp), ".png"))

the_choropleth


## ========== Generate 
## ==============================

gif_choropleth <- function(start.year,
                           end.year,
                           how.tally,
                           time.period = 10,
                           shapefile,
                           cumulative = FALSE) {
  
  unlink("gif-store/", recursive = TRUE)
  dir.create("gif-store")
  
  lapply(seq(start.year, end.year, time.period),
         function(start.year) {
           the_choropleth <- make_img_choropleth(
             start.year = start.year,
             end.year = start.year + time.period,
             how.tally = how.tally,
             shapefile = shapefile,
             cumulative = cumulative
           )
           tmp_fp <-
             paste0(
               "gif-store/",
               "choropleth_",
               shapefile,
               start.year,
               "-",
               start.year + time.period,
               ".html"
             )
           htmlwidgets::saveWidget(
             widget = the_choropleth,
             file = file.path(normalizePath(dirname(tmp_fp)), basename(tmp_fp)),
             selfcontained = FALSE
           )
           webshot::webshot(url = tmp_fp,
                            file = paste0(tools::file_path_sans_ext(tmp_fp), ".png"))
         })
  
  map_image_vector <- c()
  map_files <- list.files("gif-store/",
                          include.dirs = TRUE,
                          full.names = TRUE) %>%
    .[grepl("choropleth.*png", .)]
  
  lapply(map_files,
         function(x) {
           map_image_vector <<- append(map_image_vector, image_read(x))
         })
  
  map_image_vector %>%
    image_crop("992x630+10+90") %>%
    image_animate(fps = 1, loop = 1) %>%
    image_write(
      paste0(
        "choropleth_",
        shapefile,
        "_",
        start.year,
        "-",
        end.year,
        "_cumulative-",
        cumulative ,
        "_gif.gif"
      )
    )
  
  unlink("gif-store/", recursive = TRUE)
  
}



gif_choropleth(1810,
               1860,
               "sender",
               time.period = 10,
               "states",
               cumulative = TRUE)

gif_choropleth(1810,
               1860,
               "sender",
               time.period = 10,
               "congressional districts",
               cumulative = FALSE)

