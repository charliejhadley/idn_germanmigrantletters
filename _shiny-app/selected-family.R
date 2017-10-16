output$selected_family_UI <- renderUI({
  selectInput(
    "selected_family",
    "Choose a family of interest",
    choices = list("Selected by email" = "G009"),
    selected = "G009"
  )
})

output$selected_family_checkbox_datefilter_UI <- renderUI({
  checkboxInput("selected_family_checkbox_datefilter",
                label = "Include undated letters?",
                value = FALSE)
})

observeEvent(input$selected_family_checkbox_datefilter,
             {
               if(is.null(input$selected_family_checkbox_datefilter)){
                 return()
               }
               
               enable_date_slider <- !input$selected_family_checkbox_datefilter
               toggleState(id = "selected_family_date_slider", condition = enable_date_slider)
             }
)

output$selected_family_date_slider_ui <- renderUI({
  
  sliderInput(
    "selected_family_date_slider",
    "Date Range",
    min = min(letters_df$date, na.rm = T),
    max = max(letters_df$date, na.rm = T),
    value = c(
      min(letters_df$date, na.rm = T),
      max(letters_df$date, na.rm = T)
    )
  )
  
  
})

selected_family_letters <- eventReactive(
  c(
    input$selected_family,
    input$selected_family_date_slider,
    input$selected_family_checkbox_datefilter
  ),
  {
    switch(input$selected_family,
           "G009" = {
             selected_family_letters <- letters_df %>%
               filter(id.letter %in% gsub(".txt", "", list.files(path = "data-raw/target-family/")))
           })
    
    journeys_filtered_letters <-
      selected_family_letters %>%
      filter(!is.na(receiver.latitude) &
               !is.na(sender.location))
    
    if (input$selected_family_checkbox_datefilter) {
      journeys_filtered_letters
    } else {
      journeys_filtered_letters <- journeys_filtered_letters %>%
        filter_("!is.na(date)")
      
      min_date <-
        input$selected_family_date_slider[1]
      max_date <-
        input$selected_family_date_slider[2]
      
      journeys_filtered_letters <-
        journeys_filtered_letters %>%
        filter(date >= min_date &
                 date <= max_date)
      
      journeys_filtered_letters
    }
    
    
  }
)


output$selected_family_letters_journeys_map <- renderLeaflet({
  if (is.null(input$selected_family_checkbox_datefilter)) {
    return()
  }
  
  if (!input$selected_family_checkbox_datefilter) {
    if (is.null(input$selected_family_date_slider)) {
      return()
    }
  }
  
  if (is.null(selected_family_letters())) {
    shinyjs::show(id = "loading-selected-family",
                  anim = TRUE,
                  animType = "fade")
  }
  
  selected_family_letters <- selected_family_letters()
  
  if(!nrow(selected_family_letters) == 0){
    leaflet() %>%
      addProviderTiles("Esri.WorldShadedRelief") %>%
      addPolylines(
        data = letter_journey_lines(selected_family_letters),
        color = rgb(44, 123, 182, max = 255),
        popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
        weight = 4,
        opacity = 0.3
      ) %>%
      send_only_markers(selected_family_letters) %>%
      receive_only_markers(selected_family_letters) %>%
      two_way_markers(selected_family_letters) %>% {
        shinyjs::hide(id = "loading-selected-family",
                      anim = TRUE,
                      animType = "fade")
        addLegendCustom(
          .,
          colors = c("#fdae61", "#d7191c", "#7570b3"),
          labels = c("Sender", "Receiver", "Sender and Receiver"),
          sizes = c(10, 10, 10)
        )
      }
  } else {
    leaflet() %>%
      addProviderTiles("Esri.WorldShadedRelief") %>%
      addLegendCustom(
        .,
        colors = c("#fdae61", "#d7191c", "#7570b3"),
        labels = c("Sender", "Receiver", "Sender and Receiver"),
        sizes = c(10, 10, 10)
      )
  }
  
  
  
})

selected_family_clicked_location <-
  eventReactive(input$selected_family_letters_journeys_map_marker_click,
                {
                  selected_family_letters <- selected_family_letters()
                  
                  
                  selected_location <-
                    selected_family_letters %>%
                    mutate(
                      selected.by.send = ifelse(
                        sender.latitude == input$selected_family_letters_journeys_map_marker_click$lat &
                          sender.longitude == input$selected_family_letters_journeys_map_marker_click$lng,
                        TRUE,
                        FALSE
                      )
                    ) %>%
                    mutate(
                      selected.by.receive = ifelse(
                        receiver.latitude == input$selected_family_letters_journeys_map_marker_click$lat &
                          receiver.longitude == input$selected_family_letters_journeys_map_marker_click$lng,
                        TRUE,
                        FALSE
                      )
                    ) %>%
                    filter(selected.by.send == TRUE |
                             selected.by.receive == TRUE) %>%
                    mutate(select.loc.name = ifelse(selected.by.send == TRUE, sender.location, receiver.location)) %>%
                    select(select.loc.name) %>%
                    .[[1]] %>%
                    unique()
                })


output$selected_family_click_summary <- renderUI({
  if (is.null(input$selected_family_letters_journeys_map_marker_click)) {
    return()
  }
  
  selected_family_letters <- selected_family_letters()
  
  selected_family_clicked_location <-
    selected_family_clicked_location()
  
  nletters_associated_with_location <-
    journey_termini_data(selected_family_letters) %>%
    filter(location.name == selected_family_clicked_location) %>%
    select(total.sent, total.received) %>%
    rowSums(na.rm = TRUE)
  
  
  wellPanel(
    h6("Selected location: ", selected_family_clicked_location),
    h6(
      "Letters associated with location: ",
      nletters_associated_with_location
    ),
    h6("Scroll down for details")
  )
  
})


output$sender_letter_viewer_UI <- renderUI({
  selected_family_letters <- selected_family_letters()
  
  selected_family_clicked_location <-
    selected_family_clicked_location()
  
  nletters_sent <- selected_family_letters %>%
    filter(sender.location == selected_family_clicked_location) %>%
    nrow()
  
  
  letter_ids <- selected_family_letters %>%
    filter(sender.location == selected_family_clicked_location) %>%
    select(id.letter) %>%
    .[[1]]
  
  if (is.null(input$sender_letter_viewer$page_current)) {
    fluidPage(pageruiInput('sender_letter_viewer', 1, nletters_sent),
              hr(),
              "")
  } else {
    if (nletters_sent > 0) {
      fluidPage(
        h5(
          paste0("Letters sent from ", selected_family_clicked_location)
        ),
        pageruiInput('sender_letter_viewer', 1, nletters_sent),
        hr(),
        HTML(gsub("\n", "<p>", read_file(
          paste0("data-raw/target-family/", letter_ids[input$sender_letter_viewer$page_current], ".txt")
        )))
      )
    } else {
      fluidPage(h5("No letters were sent from this location"))
    }
  }
  
  
  
})

output$receiver_letter_viewer_UI <- renderUI({
  selected_family_letters <- selected_family_letters()
  
  selected_family_clicked_location <-
    selected_family_clicked_location()
  
  nletters_received <- selected_family_letters %>%
    filter(receiver.location == selected_family_clicked_location) %>%
    nrow()
  
  
  letter_ids <- selected_family_letters %>%
    filter(receiver.location == selected_family_clicked_location) %>%
    select(id.letter) %>%
    .[[1]]
  
  if (is.null(input$receiver_letter_viewer$page_current)) {
    fluidPage(pageruiInput('receiver_letter_viewer', 1, nletters_received),
              hr(),
              "")
  } else {
    if (nletters_received > 0) {
      fluidPage(
        h5(
          paste0(
            "Letters received from ",
            selected_family_clicked_location
          )
        ),
        pageruiInput('receiver_letter_viewer', 1, nletters_received),
        hr(),
        HTML(gsub("\n", "<p>", read_file(
          paste0("data-raw/target-family/", letter_ids[input$receiver_letter_viewer$page_current], ".txt")
        )))
      )
    } else {
      fluidPage(h5("No letters were received  this location"))
    }
  }
  
  
  
  
})


observeEvent(input$sender_letter_viewer$page_current,
             {
               selected_family_letters <- selected_family_letters()
               
               selected_family_clicked_location <-
                 selected_family_clicked_location()
               
               nletters_sent <- selected_family_letters %>%
                 filter(sender.location == selected_family_clicked_location) %>%
                 nrow()
               
               updatePageruiInput(
                 session,
                 'sender_letter_viewer',
                 page_current = input$sender_letter_viewer$page_current,
                 pages_total = nletters_sent
               )
             },
             ignoreInit = TRUE)

observeEvent(input$receiver_letter_viewer$page_current,
             {
               selected_family_letters <- selected_family_letters()
               
               selected_family_clicked_location <-
                 selected_family_clicked_location()
               
               nletters_sent <- selected_family_letters %>%
                 filter(receiver.location == selected_family_clicked_location) %>%
                 nrow()
               
               updatePageruiInput(
                 session,
                 'receiver_letter_viewer',
                 page_current = input$receiver_letter_viewer$page_current,
                 pages_total = nletters_sent
               )
             },
             ignoreInit = TRUE)


output$selected_family_letter_viewers_UI <- renderUI({
  if (is.null(input$selected_family_letters_journeys_map_marker_click)) {
    return()
  }
  
  
  fluidRow(column(uiOutput("sender_letter_viewer_UI"),
                  width = 6),
           column(uiOutput("receiver_letter_viewer_UI"),
                  width = 6))
  
  
})
