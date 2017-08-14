
gmap_selected_sender_locs <- states_shapefiles %>%
  ggplot() +
  geom_sf() + 
  geom_sf(data = unique_selected_letter_locations,
          
          colour = "#c4c4c4", size = 0.001)
  # theme(
  #   text = element_text(family = "Ubuntu Regular", color = "#22211d"),
  #   axis.line = element_blank(),
  #   axis.text.x = element_blank(),
  #   axis.text.y = element_blank(),
  #   axis.ticks = element_blank(),
  #   axis.title.x = element_blank(),
  #   axis.title.y = element_blank(),
  #   # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
  #   panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
  #   panel.grid.minor = element_blank(),
  #   plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  #   panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  #   legend.background = element_rect(fill = "#f5f5f2", color = NA),
  #   panel.border = element_blank()

geom_letter_decade_points <- function(map = map,
                                      start.time.period,
                                      end.time.period){
  
  interval_in_years <- interval(end.time.period, start.time.period) / years(1)
  
  current_decade <- selected_families_letters %>%
    filter(date >= start.time.period &
             date < end.time.period) %>%
    select(sender.latitude, sender.longitude) %>%
    unique() %>%
    st_as_sf(coords = c("sender.longitude", "sender.latitude")) %>%
    st_set_crs(st_crs(states_shapefiles))
  
  
  previous_decade <- selected_families_letters %>%
    filter(date >= start.time.period - dyears(interval_in_years) &
             date <= end.time.period - dyears(interval_in_years)) %>%
    select(sender.latitude, sender.longitude) %>%
    unique() %>%
    st_as_sf(coords = c("sender.longitude", "sender.latitude")) %>%
    st_set_crs(st_crs(states_shapefiles))
  
  map +
    geom_sf(data = previous_decade,
            color = "#fdbf6f") +
    geom_sf(data = current_decade,
            color = "#ff7f00")
}



output$selected_family_date_range_UI <- renderUI({
  
  sliderInput(
    "selected_family_date_range",
    "Date Range",
    min = year(min(selected_families_letters$date, na.rm = TRUE)),
    max = year(max(selected_families_letters$date, na.rm = TRUE)),
    value = c(
      1835,
      1845
    ),
    step = 10,
    width = "100%",
    animate = animationOptions(interval = 1500)
  )
})

output$selected_family_leaflet_map <- renderLeaflet({
  
  if(is.null(input$selected_family_date_range)){
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

