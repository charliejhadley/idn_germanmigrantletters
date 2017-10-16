output$selected_family_date_range_UI <- renderUI({
  selected_family_letters <- selected_families_letters %>%
    filter(grepl(input$selected_family_which_family, id.letter))
  
  
  sliderInput(
    "selected_family_date_range",
    "Date Range",
    # min = year(min(selected_family_letters$date, na.rm = TRUE)),
    min = floor(year(min(selected_family_letters$date, na.rm = TRUE)) / 10) * 10,
    # max = year(max(selected_family_letters$date, na.rm = TRUE)),
    max = floor(year(max(selected_family_letters$date, na.rm = TRUE)) / 10) * 10,
    value = c(floor(year(min(selected_family_letters$date, na.rm = TRUE)) / 10) * 10,
              floor(year(min(selected_family_letters$date, na.rm = TRUE)) / 10) * 10 + 10),
    step = 10,
    width = "100%",
    animate = animationOptions(interval = 3000),
    sep = ""
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
      colors = c("#fdbf6f", "#ff7f00"),
      labels = c("Previous time period", "Current time period"),
      sizes = c(10, 10),
      title = "Letters sent from location?",
      opacity = 1
    )
  
})



selected_family_CircleMarkers_data <-
  eventReactive(input$selected_family_date_range,
                {
                  selected_family_letters <- selected_families_letters %>%
                    filter(grepl(input$selected_family_which_family, id.letter))
                  
                  start.time.period <-
                    dmy(paste0("01-01", input$selected_family_date_range[1]))
                  end.time.period <-
                    dmy(paste0("01-01", input$selected_family_date_range[2]))
                  
                  interval_in_years <-
                    interval(start.time.period, end.time.period) / years(1)
                  
                  previous_decade <-
                    selected_family_letters %>%
                    filter(
                      date >= start.time.period - dyears(interval_in_years) &
                        date <= end.time.period - dyears(interval_in_years)
                    ) %>%
                    select(sender.latitude, sender.longitude) %>%
                    unique() %>%
                    mutate(type = "previous",
                           color = "#fdbf6f")
                  
                  current_decade <-
                    selected_family_letters %>%
                    filter(date >= start.time.period &
                             date <= end.time.period) %>%
                    select(sender.latitude, sender.longitude) %>%
                    unique() %>%
                    mutate(type = "current",
                           color = "#ff7f00")
                  
                  other_locations <-
                    unique_selected_letter_locations %>%
                    mutate(type = "no.sent",
                           color = "#d6d4d4") %>%
                    anti_join(current_decade,
                              by = c("sender.latitude", "sender.longitude")) %>%
                    anti_join(previous_decade,
                              by = c("sender.latitude", "sender.longitude"))
                  
                  other_locations %>%
                    full_join(previous_decade) %>%
                    full_join(current_decade) %>%
                    distinct(sender.latitude, sender.longitude, .keep_all = TRUE) %>%
                    arrange(desc(type)) # previous 
                  
                })

observeEvent(input$selected_family_date_range,
             {
               
               selected_family_CircleMarkers_data <-
                 selected_family_CircleMarkers_data() %>%
                 st_as_sf(coords = c("sender.longitude", "sender.latitude")) %>%
                 st_set_crs(st_crs(states_shapefiles))
               
               leafletProxy("selected_family_leaflet_map") %>%
                 clearMarkers() %>%
                 addCircleMarkers(
                   data = selected_family_CircleMarkers_data,
                   radius = 5,
                   weight = 1,
                   stroke = TRUE,
                   opacity = 1,
                   fillOpacity = 1,
                   fillColor = ~ color,
                   color = "#000000"
                 )
              
             })


output$sender_letter_viewer_UI <- renderUI({
  
  if(is.null(input$selected_family_leaflet_map_marker_click)){
    return()
  }
  
  clicked_marker <- input$selected_family_leaflet_map_marker_click
  
  selected_family_CircleMarkers_data <-
    selected_family_CircleMarkers_data()
  
  start.time.period <-
    dmy(paste0("01-01", input$selected_family_date_range[1]))
  end.time.period <-
    dmy(paste0("01-01", input$selected_family_date_range[2]))
  
  clicked_type <- selected_family_CircleMarkers_data %>%
    filter(sender.latitude == clicked_marker$lat,
           sender.longitude == clicked_marker$lng) %>%
    select(type) %>%
    .[[1]]
  
  selected_family_letters <- selected_families_letters %>%
    filter(grepl(
      input$selected_family_which_family, id.letter
    )) %>%
    filter(date >= start.time.period & date <= end.time.period) %>%
    filter(
      sender.latitude == clicked_marker$lat &
        sender.longitude == clicked_marker$lng
    )
  
  location_name <- locations_df %>%
    filter(latitude == clicked_marker$lat,
           longitude == clicked_marker$lng) %>%
    select(location.string) %>%
    unique() %>%
    .[[1]]
  
  nletters_sent <- selected_family_letters %>%
    nrow()
  
  letter_ids <- selected_family_letters %>%
    select(id.letter) %>%
    .[[1]]
  
  switch(clicked_type,
         "no.sent" = {
           paste("No letters were sent from", location_name, "location in the selected time period")
         },
         "previous" = {
           paste("Letters were sent from", location_name, "location in the previous time period")
         },
         "current" = {
           
           if (is.null(input$sender_letter_viewer$page_current)) {
             fluidPage(pageruiInput('sender_letter_viewer', 1, nletters_sent),
                       hr(),
                       "")
           } else {
             if (nletters_sent > 0) {
               fluidPage(
                 h5(
                   paste0("Letters sent from ", location_name)
                 ),
                 pageruiInput('sender_letter_viewer', 1, nletters_sent),
                 hr(),
                 HTML(gsub("\r", "<br>",
                           
                           #           read_file(
                           #   paste0("data-raw/target-family/", letter_ids[input$sender_letter_viewer$page_current], ".txt")
                           # )
                           #
                           text_of_letters %>%
                             filter(id == letter_ids[input$sender_letter_viewer$page_current]) %>%
                             select(truncated.text)
                 ))
               )
             }
           }
           
         })
  
  

  


  
})

observeEvent(input$sender_letter_viewer$page_current,
             {
               clicked_marker <- input$selected_family_leaflet_map_marker_click
               
               selected_family_CircleMarkers_data <-
                 selected_family_CircleMarkers_data()
               
               start.time.period <-
                 dmy(paste0("01-01", input$selected_family_date_range[1]))
               end.time.period <-
                 dmy(paste0("01-01", input$selected_family_date_range[2]))
               
               clicked_type <- selected_family_CircleMarkers_data %>%
                 filter(sender.latitude == clicked_marker$lat,
                        sender.longitude == clicked_marker$lng) %>%
                 select(type) %>%
                 .[[1]]
               
               selected_family_letters <- selected_families_letters %>%
                 filter(grepl(
                   input$selected_family_which_family, id.letter
                 )) %>%
                 filter(date >= start.time.period & date <= end.time.period) %>%
                 filter(
                   sender.latitude == clicked_marker$lat &
                     sender.longitude == clicked_marker$lng
                 )
               
               nletters_sent <- selected_family_letters %>%
                 nrow()
               
               letter_ids <- selected_family_letters %>%
                 select(id.letter) %>%
                 .[[1]]
               
               updatePageruiInput(
                 session,
                 'sender_letter_viewer',
                 page_current = input$sender_letter_viewer$page_current,
                 pages_total = nletters_sent
               )
             },
             ignoreInit = TRUE)

