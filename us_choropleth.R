## ======================== inputs
## ================================================

output$choropleth_checkbox_datefilter_UI <- renderUI({
  checkboxInput("choropleth_checkbox_datefilter",
                label = "Include undated letters?",
                value = TRUE)
})


output$choropleth_date_slider_ui <- renderUI({
  if (is.null(input$choropleth_checkbox_datefilter)) {
    return()
  }
  
  if (input$choropleth_checkbox_datefilter) {
    return()
  }
  
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

choropleth_spdf_tally <- eventReactive(
  c(input$choropleth_how_tally, input$choropleth_date_slider, input$choropleth_checkbox_datefilter),
{
  # if(is.null(input$choropleth_checkbox_datefilter)){
  #   return()
  # }
  
  choropleth_filtered_letters <- letters_df %>%
    filter(!is.na(sender.latitude)) # sender latitude must exist for this visualisation
  
  if(input$choropleth_checkbox_datefilter){
    choropleth_filtered_letters %>%
      spdf_letters(send.or.receive = input$choropleth_how_tally) %>%
      count_letters_in_states()
  } else {
    choropleth_filtered_letters %>%
      filter(!is.na(date)) %>%
      filter(date >= input$choropleth_date_slider[1] &
               date <= input$choropleth_date_slider[2]) %>%
      spdf_letters(send.or.receive = input$choropleth_how_tally) %>%
      count_letters_in_states()
  }

},
ignoreNULL = FALSE)

## ======================== us_states_choropleth
## ================================================

output$us_states_choropleth <- renderLeaflet({
  
  if(is.null(input$choropleth_checkbox_datefilter)){
    return()
  }
  
  if(is.null(choropleth_spdf_tally())){
    shinyjs::show(id = "loading-choropleth", anim = TRUE, animType = "fade")
  } else {
    shinyjs::hide(id = "loading-choropleth", anim = TRUE, animType = "fade")
  }
  
  palette <- colorBin(
    c("#cccccc", brewer.pal(5, "YlGnBu")),
    bins = c(0, 1, 5, 10, 20, 50, 350),
    pretty = FALSE,
    # na.color = "#cccccc",
    alpha = TRUE
  )
  
  region_labeller <- function(number_of_points = NA) {
    paste0(# "<p>", state_name, "</p>",
      "<p>Number of letters: ", number_of_points, "</p>")
  }
  
  choropleth_spdf_tally() %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(
      stroke = TRUE,
      color = "#ffffff",
      smoothFactor = 0.2,
      fillOpacity = 0.8,
      fillColor = ~ palette(Count.of.Send.Locations),
      weight = 1,
      popup = ~ region_labeller(number_of_points = Count.of.Send.Locations)
      # popup = ~region_labeller(state_name = State_Name, number_of_points = var)
    ) %>%
    addLegend(
      position = 'topleft',
      ## choose bottomleft, bottomright, topleft or topright
      colors = c("#cccccc", brewer.pal(5, "YlGnBu")),
      labels = c("0", "1-5", "5-10", "10-20", "20-50", "50-350"),
      ## legend labels (only min and max)
      opacity = 0.6,
      ##transparency again
      title = "relative<br>amount"
    )
  
})