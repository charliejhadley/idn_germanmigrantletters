## ======================== inputs
## ================================================

output$journeys_checkbox_datefilter_UI <- renderUI({
  checkboxInput("journeys_checkbox_datefilter",
                label = "Include undated letters?",
                value = FALSE)
})


observeEvent(input$journeys_checkbox_datefilter,
             {
               if(is.null(input$journeys_checkbox_datefilter)){
                 return()
               }
               
               enable_date_slider <- !input$journeys_checkbox_datefilter
               print('enable_date_slider')
               print(enable_date_slider)
               toggleState(id = "journeys_date_slider", condition = enable_date_slider)
             }
)

output$journeys_date_slider_ui <- renderUI({

  sliderInput(
    "journeys_date_slider",
    "Date Range",
    min = min(letters_df$date, na.rm = T),
    max = max(letters_df$date, na.rm = T),
    value = c(
      min(letters_df$date, na.rm = T),
      max(letters_df$date, na.rm = T)
    )
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
    
    min_date <- input$journeys_date_slider[1] 
    max_date <- input$journeys_date_slider[2] 
    
    journeys_filtered_letters <- journeys_filtered_letters %>%
      filter(date >= min_date &
               date <= max_date)
    
    journeys_filtered_letters
  }
  
},
ignoreNULL = FALSE)

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
    addProviderTiles("Esri.WorldShadedRelief") %>%
    addPolylines(
      data = letter_journey_lines(journeys_filtered_letters),
      color = rgb(44, 123, 182, max = 255),
      popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
      weight = 4,
      opacity = 0.3
    ) %>%
    send_only_markers(journeys_filtered_letters) %>%
    receive_only_markers(journeys_filtered_letters) %>%
    two_way_markers(journeys_filtered_letters) %>% {
      shinyjs::hide(id = "loading-journeys",
                    anim = TRUE,
                    animType = "fade")
      addLegendCustom(
        .,
        colors = c("#fdae61", "#d7191c", "#7570b3"),
        labels = c("Sender", "Receiver", "Sender and Receiver"),
        sizes = c(10, 10, 10)
      )
    }
  
})