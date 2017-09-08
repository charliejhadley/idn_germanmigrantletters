## ======================== inputs
## ================================================

output$journeys_checkbox_datefilter_UI <- renderUI({
  checkboxInput("journeys_checkbox_datefilter",
                label = "Include undated letters?",
                value = FALSE)
})


observeEvent(input$journeys_checkbox_datefilter,
             {
               if (is.null(input$journeys_checkbox_datefilter)) {
                 return()
               }
               
               enable_date_slider <-
                 !input$journeys_checkbox_datefilter
               print('enable_date_slider')
               print(enable_date_slider)
               toggleState(id = "journeys_date_slider", condition = enable_date_slider)
             })

output$journeys_date_slider_ui <- renderUI({
  sliderInput(
    "journeys_date_slider",
    "Date Range",
    min = year(min(letters_df$date, na.rm = T)),
    max = year(max(letters_df$date, na.rm = T)),
    value = c(
      year(min(letters_df$date, na.rm = T)),
      year(max(letters_df$date, na.rm = T))
    ),
    sep = ""
  )
  
})


## ======================== journeys_filtered_letters
## ================================================

journeys_filtered_letters <- eventReactive(c(
  input$journeys_date_slider,
  input$journeys_checkbox_datefilter
),
{
  # if(is.null(input$choropleth_checkbox_datefilter)){
  #   return()
  # }
  
  journeys_filtered_letters <- letters_df %>%
    filter(!is.na(receiver.latitude) &
             !is.na(sender.location))
  
  if (input$journeys_checkbox_datefilter) {
    journeys_filtered_letters
  } else {
    journeys_filtered_letters <- journeys_filtered_letters %>%
      filter_("!is.na(date)")
    
    
    
    min_date <- dmy(paste0("01-01-",input$journeys_date_slider[1]))
    max_date <- dmy(paste0("01-01-",input$journeys_date_slider[2]))
    
    journeys_filtered_letters <- journeys_filtered_letters %>%
      filter(date >= min_date &
               date <= max_date)
    
    journeys_filtered_letters
  }
  
},
ignoreNULL = FALSE)

## ======================== letter_journeys_map
## ================================================


output$letter_journeys_selected_family_UI <- renderUI({
  
  if(isTRUE(input$highlight_selected_families)){
    selectInput(
      "letter_journeys_selected_family",
      "Highlight a family:",
      choices = selected_family_letter_series
    )
  }
  
})

observeEvent(c(input$highlight_selected_families,
               input$letter_journeys_selected_family,
               input$journeys_date_slider,
               input$journeys_checkbox_datefilter),
             {
               
               if (is.null(input$journeys_checkbox_datefilter)) {
                 return()
               }
               
               if (!input$journeys_checkbox_datefilter) {
                 if (is.null(input$journeys_date_slider)) {
                   return()
                 }
               }
               
               if (is.null(journeys_filtered_letters())) {
                 return()
               }
               journeys_filtered_letters <- journeys_filtered_letters()

               
               if(isTRUE(input$highlight_selected_families)){
                 
                 if(is.null(input$letter_journeys_selected_family)){
                   return()
                 }
                 
                 selected_family_journeys <- journeys_filtered_letters %>%
                   filter(grepl(input$letter_journeys_selected_family, id.letter)) %>%
                   letter_journey_lines()
                 
                 if(nrow(selected_family_journeys) == 0){
                   leafletProxy("letter_journeys_map") %>%
                     clearShapes() %>%
                     clearMarkers() %>%
                     removeControl("legend") %>%
                     send_only_markers(journeys_filtered_letters) %>%
                     receive_only_markers(journeys_filtered_letters) %>%
                     two_way_markers(journeys_filtered_letters) %>%
                     addPolylines(
                       data = journeys_filtered_letters %>%
                         filter(!grepl(input$letter_journeys_selected_family, id.letter)) %>%
                         letter_journey_lines(),
                       color = rgb(166, 206, 227, max = 255),
                       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
                       weight = 4,
                       opacity = 0.3
                     ) %>%
                     addLegendCustom(
                       .,
                       colors = c("#fdae61", "#d7191c", "#7570b3", "#1f78b4", "#a6cee3"),
                       labels = c("Sender", "Receiver", "Sender and Receiver", "Selected family", "Other family"),
                       sizes = c(10, 10, 10),
                       layerId = "legend"
                     )
                 } else {
                   leafletProxy("letter_journeys_map") %>%
                     clearShapes() %>%
                     clearMarkers() %>%
                     removeControl("legend") %>%
                     send_only_markers(journeys_filtered_letters) %>%
                     receive_only_markers(journeys_filtered_letters) %>%
                     two_way_markers(journeys_filtered_letters) %>%
                     addPolylines(
                       data = journeys_filtered_letters %>%
                         filter(!grepl(input$letter_journeys_selected_family, id.letter)) %>%
                         letter_journey_lines(),
                       color = rgb(166, 206, 227, max = 255),
                       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
                       weight = 4,
                       opacity = 0.3
                     ) %>%
                     addPolylines(
                       data = journeys_filtered_letters %>%
                         filter(grepl(input$letter_journeys_selected_family, id.letter)) %>%
                         letter_journey_lines(),
                       color = rgb(31, 120, 180, max = 255),
                       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
                       weight = 4,
                       opacity = 1
                     ) %>%
                     addLegendCustom(
                       .,
                       colors = c("#fdae61", "#d7191c", "#7570b3", "#1f78b4", "#a6cee3"),
                       labels = c("Sender", "Receiver", "Sender and Receiver", "Selected family", "Other family"),
                       sizes = c(10, 10, 10),
                       layerId = "legend"
                     )
                 }
                 
               } else {
                 leafletProxy("letter_journeys_map") %>%
                   clearShapes() %>%
                   clearMarkers() %>%
                   send_only_markers(journeys_filtered_letters) %>%
                   receive_only_markers(journeys_filtered_letters) %>%
                   two_way_markers(journeys_filtered_letters) %>%
                   addPolylines(
                     data = letter_journey_lines(journeys_filtered_letters),
                     color = rgb(44, 123, 182, max = 255),
                     popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
                     weight = 4,
                     opacity = 0.3
                   ) %>%
                   addLegendCustom(
                     .,
                     colors = c("#fdae61", "#d7191c", "#7570b3"),
                     labels = c("Sender", "Receiver", "Sender and Receiver"),
                     sizes = c(10, 10, 10),
                     layerId = "legend"
                   )
               }
               
             })

output$letter_journeys_map <- renderLeaflet({
  if (is.null(input$journeys_checkbox_datefilter)) {
    return()
  }
  
  if (!input$journeys_checkbox_datefilter) {
    if (is.null(input$journeys_date_slider)) {
      return()
    }
  }
  
  if (is.null(journeys_filtered_letters())) {
    shinyjs::show(id = "loading-journeys",
                  anim = TRUE,
                  animType = "fade")
  }
  
  journeys_filtered_letters <- journeys_filtered_letters()
  
  shinyjs::hide(id = "loading-journeys",
                anim = TRUE,
                animType = "fade")
  print(head(journeys_filtered_letters))
  
  leaflet() %>%
    addProviderTiles("Esri.WorldShadedRelief") %>%
    send_only_markers(journeys_filtered_letters) %>%
    receive_only_markers(journeys_filtered_letters) %>%
    two_way_markers(journeys_filtered_letters) %>%
    addPolylines(
      data = letter_journey_lines(journeys_filtered_letters),
      color = rgb(44, 123, 182, max = 255),
      popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
      weight = 4,
      opacity = 0.3
    ) %>%
    addLegendCustom(
      .,
      colors = c("#fdae61", "#d7191c", "#7570b3"),
      labels = c("Sender", "Receiver", "Sender and Receiver"),
      sizes = c(10, 10, 10),
      layerId = "legend"
    )
  
  # 
  # if(isTRUE(input$highlight_selected_families)){
  #   
  #   if(is.null(input$letter_journeys_selected_family)){
  #     return()
  #   }
  #   
  #   
  #   leaflet() %>%
  #     addProviderTiles("Esri.WorldShadedRelief") %>%
  #     addPolylines(
  #       data = letters_df %>%
  #         filter(!grepl(input$letter_journeys_selected_family, id.letter)) %>%
  #         letter_journey_lines(),
  #       color = rgb(166, 206, 227, max = 255),
  #       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #       weight = 4,
  #       opacity = 0.3
  #     ) %>%
  #     addPolylines(
  #       data = letters_df %>%
  #         filter(grepl(input$letter_journeys_selected_family, id.letter)) %>%
  #         letter_journey_lines(),
  #       color = rgb(31, 120, 180, max = 255),
  #       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #       weight = 4,
  #       opacity = 1
  #     )
  #   
  # } else {
  #   leaflet() %>%
  #     addProviderTiles("Esri.WorldShadedRelief") %>%
  #     addPolylines(
  #       data = letter_journey_lines(journeys_filtered_letters),
  #       color = rgb(44, 123, 182, max = 255),
  #       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #       weight = 4,
  #       opacity = 0.3
  #     ) %>%
  #     send_only_markers(journeys_filtered_letters) %>%
  #     receive_only_markers(journeys_filtered_letters) %>%
  #     two_way_markers(journeys_filtered_letters) %>% 
  #     addLegendCustom(
  #       .,
  #       colors = c("#fdae61", "#d7191c", "#7570b3"),
  #       labels = c("Sender", "Receiver", "Sender and Receiver"),
  #       sizes = c(10, 10, 10)
  #     )
  # }
  
  
  
  # 
  # if(isTRUE(input$highlight_selected_families)){
  #   
  #   leaflet() %>%
  #     addProviderTiles("Esri.WorldShadedRelief") %>%
  #     addPolylines(
  #       data = letter_journey_lines(letters_df, selected.family = "exclude"),
  #       color = rgb(166, 206, 227, max = 255),
  #       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #       weight = 4,
  #       opacity = 0.3
  #     ) %>%
  #     addPolylines(
  #       data = letter_journey_lines(letters_df, selected.family = "only"),
  #       color = rgb(31, 120, 180, max = 255),
  #       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #       weight = 4,
  #       opacity = 1
  #     ) %>%
  #     send_only_markers(journeys_filtered_letters) %>%
  #     receive_only_markers(journeys_filtered_letters) %>%
  #     two_way_markers(journeys_filtered_letters) %>% 
  #     addLegendCustom(
  #       .,
  #       colors = c("#fdae61", "#d7191c", "#7570b3"),
  #       labels = c("Sender", "Receiver", "Sender and Receiver"),
  #       sizes = c(10, 10, 10)
  #     )
  #   
  #     
  #     # leaflet() %>%
  #     #   addProviderTiles("Esri.WorldShadedRelief") %>%
  #     #   addPolylines(
  #     #     data = letter_journey_lines(letters_df),
  #     #     color = ~ifelse(selected.family == TRUE, rgb(31, 120, 180, max = 255), rgb(166, 206, 227, max = 255)),
  #     #     popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #     #     weight = 4,
  #     #     opacity = ~ifelse(selected.family == TRUE, 1, 0.3)
  #     #   )
  #   
  #   
  #   
  # } else {
  #   leaflet() %>%
  #     addProviderTiles("Esri.WorldShadedRelief") %>%
  #     addPolylines(
  #       data = letter_journey_lines(journeys_filtered_letters),
  #       color = rgb(44, 123, 182, max = 255),
  #       popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #       weight = 4,
  #       opacity = 0.3
  #     ) %>%
  #     send_only_markers(journeys_filtered_letters) %>%
  #     receive_only_markers(journeys_filtered_letters) %>%
  #     two_way_markers(journeys_filtered_letters) %>% 
  #     addLegendCustom(
  #       .,
  #       colors = c("#fdae61", "#d7191c", "#7570b3"),
  #       labels = c("Sender", "Receiver", "Sender and Receiver"),
  #       sizes = c(10, 10, 10)
  #     )
  # }
  


  
  # leaflet() %>%
  #   addProviderTiles("Esri.WorldShadedRelief") %>%
  #   addPolylines(
  #     data = letter_journey_lines(journeys_filtered_letters),
  #     color = rgb(44, 123, 182, max = 255),
  #     popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
  #     weight = 4,
  #     opacity = 0.3
  #   ) %>%
  #   send_only_markers(journeys_filtered_letters) %>%
  #   receive_only_markers(journeys_filtered_letters) %>%
  #   two_way_markers(journeys_filtered_letters) %>% {
  #     shinyjs::hide(id = "loading-journeys",
  #                   anim = TRUE,
  #                   animType = "fade")
  #     addLegendCustom(
  #       .,
  #       colors = c("#fdae61", "#d7191c", "#7570b3"),
  #       labels = c("Sender", "Receiver", "Sender and Receiver"),
  #       sizes = c(10, 10, 10)
  #     )
  
  
  
})
