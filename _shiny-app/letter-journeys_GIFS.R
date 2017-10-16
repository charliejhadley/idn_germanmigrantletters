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

## ========== Utility functions
## ==============================

letter_journies_legend_title <- function(start.year,
                                    end.year,
                                    cumulative){
  
  if(cumulative == TRUE){
    paste0("Letter journies", "<br>",
           "Before ", end.year)
  } else {
    paste0("Letter journies", "<br>",
           start.year, " - ", end.year)
  }
  
}

make_img_letter_journies <- function(start.year,
                                end.year,
                                cumulative = FALSE) {
  journeys_filtered_letters <- letters_df %>%
    filter(!is.na(receiver.latitude) &
             !is.na(location.sender))
  
  if (cumulative == TRUE) {
    journeys_filtered_letters <- journeys_filtered_letters %>%
      filter(!is.na(date)) %>%
      filter(date <= dmy(paste0("31-12-", end.year)))
    
  } else{
    journeys_filtered_letters <- journeys_filtered_letters %>%
      filter(!is.na(date)) %>%
      filter(date >= dmy(paste0("01-01-", start.year)) &
               date <= dmy(paste0("31-12-", end.year)))
    
  }
  
  leaflet() %>%
    addProviderTiles(providers$Esri.WorldShadedRelief) %>%
    send_only_markers(journeys_filtered_letters) %>%
    receive_only_markers(journeys_filtered_letters) %>%
    two_way_markers(journeys_filtered_letters) %>%
    addPolylines(
      data = journeys_filtered_letters %>%
        letter_journey_lines(),
      color = rgb(31, 120, 180, max = 255),
      popup = ~ label_journey(location.sender, location.receiver, number.of.letters),
      weight = 4,
      opacity = 1
    ) %>%
    addLegendCustom(
      .,
      colors = c("#fdae61", "#d7191c", "#7570b3", "#1f78b4", "#a6cee3"),
      labels = c(
        "Sender",
        "Receiver",
        "Sender and Receiver",
        "Selected family",
        "Other family"
      ),
      sizes = c(10, 10, 10),
      layerId = "legend",
      title = letter_journies_legend_title(start.year, end.year, cumulative)
    ) %>%
    my_fitBounds(bbox_letter_journeys)
  
}

## ========== Single copy 
## ==============================

make_img_letter_journies(1810,
                    1820,
                    cumulative = TRUE)

## ========== Generate 
## ==============================

gif_letter_journies <- function(start.year,
                           end.year,
                           time.period = 10,
                           cumulative = FALSE) {
  
  unlink("gif-store/", recursive = TRUE)
  dir.create("gif-store")
  
  lapply(seq(start.year, end.year, time.period),
         function(start.year) {
           the_letter_journies <- make_img_letter_journies(
             start.year = start.year,
             end.year = start.year + time.period,
             cumulative = cumulative
           )
           tmp_fp <-
             paste0(
               "gif-store/",
               "choropleth_",
               start.year,
               "-",
               start.year + time.period,
               ".html"
             )
           htmlwidgets::saveWidget(
             widget = the_letter_journies,
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

  map_image_vector
    
  map_image_vector %>%
    image_animate(fps = 1, loop = 1) %>%
    image_write(
      paste0(
        "letter-journies_",
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

gif_letter_journies(1810, 1960, time.period = 10, cumulative = TRUE)

gif_letter_journies(1810, 1960, time.period = 10, cumulative = FALSE)




