## ======================== inputs
## ================================================

output$choropleth_checkbox_datefilter_UI <- renderUI({
  checkboxInput("choropleth_checkbox_datefilter",
                label = "Include undated letters?",
                value = TRUE)
})

observeEvent(input$choropleth_checkbox_datefilter,
             {
               if (is.null(input$choropleth_checkbox_datefilter)) {
                 return()
               }
               
               enable_date_slider <-
                 !input$choropleth_checkbox_datefilter
               print('enable_date_slider')
               print(enable_date_slider)
               toggleState(id = "choropleth_date_slider", condition = enable_date_slider)
             })

output$choropleth_date_slider_ui <- renderUI({
  sliderInput(
    "choropleth_date_slider",
    "Date Range",
    min = min(letters_df$date, na.rm = T),
    max = max(letters_df$date, na.rm = T),
    value = c(
      min(letters_df$date, na.rm = T),
      max(letters_df$date, na.rm = T)
    )
  )
})

## ======================== choropleth_filtered_letters
## ================================================

# choropleth_spdf_tally <- eventReactive(
#   c(input$choropleth_how_tally, input$choropleth_date_slider, input$choropleth_checkbox_datefilter, input$type_of_region),
# {
#
#   choropleth_filtered_letters <- letters_df %>%
#     filter(!is.na(sender.latitude)) # sender latitude must exist for this visualisation
#
#   if(input$choropleth_checkbox_datefilter){
#     choropleth_filtered_letters <- choropleth_filtered_letters %>%
#       spdf_letters(send.or.receive = input$choropleth_how_tally) %>%
#       count_letters_in_regions(shape.files = switch(input$type_of_region,
#                                                     "states" = states_shapefiles,
#                                                     "counties" = counties_shapefiles,
#                                                     "congressional districts" = congressional_districts_shapefiles))
#   } else {
#     choropleth_filtered_letters <- choropleth_filtered_letters %>%
#       filter(!is.na(date)) %>%
#       filter(date >= input$choropleth_date_slider[1] &
#                date <= input$choropleth_date_slider[2])
#
#     if(nrow(choropleth_filtered_letters) != 0){
#       choropleth_filtered_letters %>%
#       spdf_letters(send.or.receive = input$choropleth_how_tally) %>%
#         count_letters_in_regions(shape.files = switch(input$type_of_region,
#                                                       "states" = states_shapefiles,
#                                                       "counties" = counties_shapefiles,
#                                                       "congressional districts" = congressional_districts_shapefiles))
#     } else {
#       NA # no letters
#     }
#
#   }
#
# },
# ignoreNULL = FALSE)

choropleth_spdf_tally <- eventReactive(
  c(
    input$choropleth_how_tally,
    input$choropleth_date_slider,
    input$choropleth_checkbox_datefilter,
    input$choropleth_boundaries_to_show
  ),
  {
    choropleth_filtered_letters <- letters_df %>%
      filter(!is.na(sender.latitude)) # sender latitude must exist for this visualisation
    
    
    if (input$choropleth_checkbox_datefilter) {
      choropleth_spdf_letters <- choropleth_filtered_letters %>%
        spdf_letters(send.or.receive = input$choropleth_how_tally)
      
      
    } else {
      choropleth_filtered_letters <- choropleth_filtered_letters %>%
        filter(!is.na(date)) %>%
        filter(date >= input$choropleth_date_slider[1] &
                 date <= input$choropleth_date_slider[2])
      
      
      if (nrow(choropleth_filtered_letters) != 0) {
        choropleth_spdf_letters <- choropleth_filtered_letters %>%
          spdf_letters(send.or.receive = input$choropleth_how_tally)
      } else {
        choropleth_spdf_letters <- NA # no letters
      }
    }
    
    print("choropleth_boundaries_to_show")
    print(input$choropleth_boundaries_to_show)
    
    if (class(choropleth_spdf_letters) == "SpatialPointsDataFrame") {
      choropleth_spdf_letters %>%
        count_letters_in_regions(shape.files = switch(
          input$choropleth_boundaries_to_show,
          "states" = states_shapefiles,
          "counties" = counties_shapefiles,
          "congressional districts" = congressional_districts_shapefiles
        ))
    } else {
      NA # no letters in range
    }
    
    
  },
  ignoreNULL = FALSE
)


# choropleth_spdf_tally <- eventReactive(
#   c(input$type_of_region),
#   {
#
#     if(class(choropleth_spdf_letters()) == "SpatialPointsDataFrame"){
#
#       choropleth_spdf_letters() %>%
#         count_letters_in_regions(shape.files = switch(input$type_of_region,
#                                                       "states" = states_shapefiles,
#                                                       "counties" = counties_shapefiles,
#                                                       "congressional districts" = congressional_districts_shapefiles))
#     } else {
#       NA # no letters in range
#     }
#   },
#   ignoreNULL = FALSE)




## ======================== us_states_choropleth
## ================================================


output$us_states_choropleth <- renderLeaflet({
  if (is.null(input$choropleth_checkbox_datefilter)) {
    return()
  }
  
  if (!input$choropleth_checkbox_datefilter) {
    if (is.null(input$choropleth_date_slider)) {
      return()
    }
  }
  
  if (is.null(choropleth_spdf_tally())) {
    shinyjs::show(id = "loading-choropleth",
                  anim = TRUE,
                  animType = "fade")
  } else {
    shinyjs::hide(id = "loading-choropleth",
                  anim = TRUE,
                  animType = "fade")
  }
  
  palette <- colorBin(
    c("#cccccc", brewer.pal(5, "YlGnBu")),
    bins = c(0, 1, 5, 10, 20, 50, 350),
    pretty = FALSE
  )
  
  region_labeller <- function(number_of_points = NA, state_name) {
    paste0("<p>",
           state_name,
           "</p>",
           "<p>Number of letters: ",
           number_of_points,
           "</p>")
  }
  
  choropleth_spdf_tally <- choropleth_spdf_tally()
  
  bounds <-
    c(-125, 24 , -75, 45) # http://rpubs.com/bhaskarvk/proj4leaflet
  
  if (class(choropleth_spdf_tally) == "SpatialPolygonsDataFrame") {
    
    base_map <- choropleth_spdf_tally() %>%
      leaflet() %>%
      addPolygons(
        stroke = TRUE,
        color = "#ffffff",
        smoothFactor = 0.2,
        fillOpacity = 0.8,
        fillColor = ~ palette(Count.of.Send.Locations),
        weight = 1,
        popup = ~ region_labeller(number_of_points = Count.of.Send.Locations, state_name = name)
        # popup = ~region_labeller(state_name = State_Name, number_of_points = var)
      ) %>%
      addLegend(
        position = 'bottomright',
        ## choose bottomleft, bottomright, topleft or topright
        colors = c("#cccccc", brewer.pal(5, "YlGnBu")),
        labels = c("0", "1-5", "5-10", "10-20", "20-50", "50-350"),
        ## legend labels (only min and max)
        opacity = 0.6,
        ##transparency again
        title = "Total letters"
      )
    
    
    switch(
      input$choropleth_boundaries_to_show,
      "states" = {
        base_map
      },
      "counties" = {
        base_map %>%
          addPolygons(
            data = state_outline_only,
            color = "#000000",
            weight = 1,
            fill = FALSE
          )
      },
      "congressional districts" = {
        base_map %>%
          addPolygons(
            data = state_outline_only,
            color = "#000000",
            weight = 1,
            fill = FALSE
          )
      }
    )
    
  } else {
    leaflet(state_outline_only) %>%
      addTiles() %>%
      addPolygons(
        stroke = TRUE,
        color = "#808080",
        smoothFactor = 0.2,
        fillOpacity = 0.8,
        weight = 1
      ) %>%
      addLegend(
        position = 'bottomright',
        colors = c("#cccccc", brewer.pal(5, "YlGnBu")),
        labels = c("0", "1-5", "5-10", "10-20", "20-50", "50-350"),
        opacity = 0.6,
        title = "relative<br>amount"
      )
    # fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    # setMaxBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  }
  
  
})


