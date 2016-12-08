## ======================== inputs
## ================================================

output$journeys_checkbox_datefilter_UI <- renderUI({
  checkboxInput("journeys_checkbox_datefilter",
                label = "Include undated letters?",
                value = TRUE)
})

output$journeys_date_slider_ui <- renderUI({
  if (is.null(input$journeys_checkbox_datefilter)) {
    return()
  }
  
  if (input$journeys_checkbox_datefilter) {
    return()
  }
  
  # sliderInput(
  #   "journeys_date_slider",
  #   "Date Range",
  #   min = min(letters_df$date, na.rm = T),
  #   max = max(letters_df$date, na.rm = T),
  #   value = c(
  #     min(letters_df$date, na.rm = T),
  #     max(letters_df$date, na.rm = T)
  #   )
  # )
  
  dateRangeInput(
    'journeys_date_slider',
    label = 'Date range input: yyyy-mm-dd',
    start = min(letters_df$date, na.rm = T),
    end = max(letters_df$date, na.rm = T)
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
    journeys_filtered_letters %>%
      filter(!is.na(date)) %>%
      filter(date >= input$journeys_date_slider[1] &
               date <= input$journeys_date_slider[2])
  }
  
},
ignoreNULL = FALSE)

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

## ======================== letter_journeys_map
## ================================================

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
  
  leaflet() %>%
    addTiles() %>%
    addPolylines(
      data = letter_journey_lines(journeys_filtered_letters),
      color = "#2c7bb6",
      popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
      weight = 4
    ) %>%
    addCircleMarkers(
      data = journey_termini_data(journeys_filtered_letters, send.or.receive = "sender"),
      lng = ~ sender.longitude,
      lat = ~ sender.latitude,
      fill = FALSE,
      radius = 1.8,
      stroke = TRUE,
      color = "#fdae61",
      opacity = 0.6
    ) %>%
    addCircleMarkers(
      data = journey_termini_data(journeys_filtered_letters, send.or.receive = "receiver"),
      lng = ~ receiver.longitude,
      lat = ~ receiver.latitude,
      fill = TRUE,
      radius = 1.8,
      stroke = TRUE,
      color = "#d7191c",
      opacity = 0.6
    ) %>% {
      shinyjs::hide(id = "loading-journeys",
                    anim = TRUE,
                    animType = "fade")
      addLegendCustom(
        .,
        colors = c("#fdae61", "#d7191c"),
        labels = c("Sender", "Receiver"),
        sizes = c(10, 10)
      )
    }
  
})