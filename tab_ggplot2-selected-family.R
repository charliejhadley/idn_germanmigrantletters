output$selected_family_date_range_UI <- renderUI({
  selected_family_letters <- selected_families_letters %>%
    filter(grepl(input$selected_family_which_family, id.letter))
  
  
  sliderInput(
    "selected_family_date_range",
    "Date Range",
    min = year(min(selected_family_letters$date, na.rm = TRUE)),
    max = year(max(selected_family_letters$date, na.rm = TRUE)),
    value = c(year(
      min(selected_family_letters$date, na.rm = TRUE)
    ),
    year(
      min(selected_family_letters$date, na.rm = TRUE)
    ) + 10),
    step = 10,
    width = "100%",
    animate = animationOptions(interval = 3000)
  )
})

output$selected_family_which_family_UI <- renderUI({
  selectInput("selected_family_which_family",
              "Which family to show?",
              choices = selected_family_letter_series)
  
})

output$selected_family_leaflet_map <- renderLeaflet({
  if (is.null(input$selected_family_date_range)) {
    return()
  }
  
  leaflet() %>%
    addPolygons(
      data = states_shapefiles,
      stroke = TRUE,
      color = "#000000",
      fillColor = "#ebebeb",
      smoothFactor = 0.2,
      fillOpacity = 0.8,
      weight = 1
    ) %>%
    addLegendCustom(
      colors = c("#fdbf6f", "#ff7f00", "#c4c4c4"),
      labels = c("Previous time period", "Current time period", "No letters sent"),
      sizes = c(10, 10, 10),
      title = "Letters sent from location?",
      opacity = 1
    )
  
})

observeEvent(input$selected_family_date_range,
             {
               start.time.period <- dmy(paste0("01-01", input$selected_family_date_range[1]))
               end.time.period <- dmy(paste0("01-01", input$selected_family_date_range[2]))
               
               interval_in_years <- interval(start.time.period, end.time.period) / years(1)
               
               all_selected_family_send_locations <- selected_families_letters %>%
                 select(sender.latitude, sender.longitude) %>%
                 unique()
               
               current_decade <- selected_families_letters %>%
                 filter(date >= start.time.period &
                          date < end.time.period) %>%
                 select(sender.latitude, sender.longitude) %>%
                 unique()
               
               previous_decade <- selected_families_letters %>%
                 filter(date >= start.time.period - dyears(interval_in_years) &
                          date <= end.time.period - dyears(interval_in_years)) %>%
                 select(sender.latitude, sender.longitude) %>%
                 unique()
               
               current_decade <- current_decade %>%
                 anti_join(previous_decade)
               
               other_locations <- all_selected_family_send_locations %>%
                 anti_join(current_decade) %>%
                 anti_join(previous_decade)
               
               current_decade <- current_decade %>%
                 st_as_sf(coords = c("sender.longitude", "sender.latitude")) %>%
                 st_set_crs(st_crs(states_shapefiles))
               
               previous_decade <- previous_decade %>%
                 st_as_sf(coords = c("sender.longitude", "sender.latitude")) %>%
                 st_set_crs(st_crs(states_shapefiles))
               
               other_locations <- other_locations %>%
                 st_as_sf(coords = c("sender.longitude", "sender.latitude")) %>%
                 st_set_crs(st_crs(states_shapefiles))
               
               
               leafletProxy("selected_family_leaflet_map") %>%
                 clearMarkers() %>%
                 addCircleMarkers(data = other_locations,
                                  radius = 3,
                                  stroke = TRUE,
                                  opacity = 1,
                                  color = "#c4c4c4") %>%
                 addCircleMarkers(data = previous_decade,
                                  radius = 3,
                                  stroke = TRUE,
                                  opacity = 1,
                                  color = "#fdbf6f") %>%
                 addCircleMarkers(data = current_decade,
                                  radius = 3,
                                  stroke = TRUE,
                                  opacity = 1,
                                  color = "#ff7f00")
               
             })

output$selected_family_ggplot_map <- renderPlot({
  
  start_time_period <- dmy(paste0("01-01", input$selected_family_date_range[1]))
  end.timeperiod <- dmy(paste0("01-01", input$selected_family_date_range[2]))
  
  geom_letter_decade_points(map = gmap_selected_sender_locs,
                            start.time.period = start_time_period,
                            end.time.period = end.timeperiod)
  
})

