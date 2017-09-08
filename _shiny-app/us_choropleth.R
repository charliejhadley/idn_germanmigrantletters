## ======================== inputs
## ================================================

output$choropleth_checkbox_datefilter_UI <- renderUI({
  checkboxInput("choropleth_checkbox_datefilter",
                label = "Include undated letters?",
                value = FALSE)
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
  
  year(min(letters_df$date, na.rm = T))
  
  sliderInput(
    "choropleth_date_slider",
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

choropleth_sf_tally <- eventReactive(
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
      choropleth_filtered_letters <- choropleth_filtered_letters
    } else {
      
      print(dmy(paste0("01-01-",input$choropleth_date_slider[1])))
      
      choropleth_filtered_letters <- choropleth_filtered_letters %>%
        filter(!is.na(date)) %>%
        filter(date >= dmy(paste0("01-01-",input$choropleth_date_slider[1])) &
                 date <= dmy(paste0("31-12-",input$choropleth_date_slider[2])))
    }
    
    selected_shapefile <- switch(
      input$choropleth_boundaries_to_show,
      "states" = states_shapefiles,
      "counties" = counties_shapefiles,
      "congressional districts" = congressional_districts_shapefiles
    )

    count_letters_in_shp(choropleth_filtered_letters, selected_shapefile)
    
  }
)

## ======================== us_states_choropleth
## ================================================

bounds <-
  c(-120, 26 , -75, 47) # http://rpubs.com/bhaskarvk/proj4leaflet

observeEvent(c(input$choropleth_date_slider,
               input$choropleth_checkbox_datefilter,
               input$choropleth_how_tally,
               input$choropleth_boundaries_to_show),
             {
               if (is.null(input$choropleth_checkbox_datefilter)) {
                 return()
               }
               
               if (!input$choropleth_checkbox_datefilter) {
                 if (is.null(input$choropleth_date_slider)) {
                   return()
                 }
               }
               
               
               if (is.null(choropleth_sf_tally())) {
                 shinyjs::show(id = "loading-choropleth",
                               anim = TRUE,
                               animType = "fade")
               } else {
                 shinyjs::hide(id = "loading-choropleth",
                               anim = TRUE,
                               animType = "fade")
               }
               
               us_palette <- colorBin(
                 c("#cccccc", brewer.pal(5, "YlGnBu")),
                 bins = c(0, 1, 5, 10, 20, 50, 350),
                 pretty = FALSE
               )
               
               choropleth_sf_tally <- choropleth_sf_tally()
               
               the_fillColor <- switch(
                 input$choropleth_how_tally,
                 "sender" = "send.count",
                 "receiver" = "received.count",
                 "both" = "total.count"
               )
               
               the_title_legend <- switch(
                 input$choropleth_how_tally,
                 "sender" = "Letters Sent",
                 "receiver" = "Letters Received",
                 "both" = "Letters Sent & Received"
               )

               switch(
                 input$choropleth_boundaries_to_show,
                 "states" = {
                   leafletProxy("us_states_choropleth") %>%
                     clearShapes() %>%
                     removeControl("legend") %>%
                     addPolygons(
                       data = choropleth_sf_tally,
                       fillColor = as.formula(paste0("~us_palette(",the_fillColor, ")")),
                       stroke = TRUE,
                       color = "#ffffff",
                       smoothFactor = 0.2,
                       fillOpacity = 0.8,
                       weight = 1,
                       popup = ~paste0(state.name,
                                       "</br>",
                                       "Letter sent: ",
                                       send.count,
                                       "</br>",
                                       "Letter received: ",
                                       received.count)
                     ) %>%
                     fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
                   addLegend(
                     position = 'bottomright',
                     pal = us_palette,
                     values = choropleth_sf_tally[[the_fillColor]],
                     title = the_title_legend,
                     layerId = "legend"
                   )
                 },
                 "congressional districts" = {
                   leafletProxy("us_states_choropleth") %>%
                     clearShapes() %>%
                     removeControl("legend") %>%
                     addPolygons(
                       data = choropleth_sf_tally,
                       fillColor = as.formula(paste0("~us_palette(",the_fillColor, ")")),
                       stroke = TRUE,
                       color = "#ffffff",
                       smoothFactor = 0.2,
                       fillOpacity = 0.8,
                       weight = 1,
                       popup = ~paste0(
                         district.name,
                         "</br>",
                         state.name,
                         "</br>",
                         "Letter sent: ",
                         send.count,
                         "</br>",
                         "Letter received: ",
                         received.count)
                     ) %>%
                     addPolygons(
                       data = states_shapefiles,
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 0,
                       options = pathOptions(clickable = FALSE)) %>%
                     fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
                   addLegend(
                     position = 'bottomright',
                     pal = us_palette,
                     values = choropleth_sf_tally[[the_fillColor]],
                     title = the_title_legend,
                     layerId = "legend"
                   )
                 },
                 "counties" = {
                   leafletProxy("us_states_choropleth") %>%
                     clearShapes() %>%
                     removeControl("legend") %>%
                     addPolygons(
                       data = choropleth_sf_tally,
                       fillColor = as.formula(paste0("~us_palette(",the_fillColor, ")")),
                       stroke = TRUE,
                       color = "#ffffff",
                       smoothFactor = 0.2,
                       fillOpacity = 0.8,
                       weight = 1,
                       popup = ~paste0(
                         county.name,
                         "</br>",
                         state.name,
                         "</br>",
                         "Letter sent: ",
                         send.count,
                         "</br>",
                         "Letter received: ",
                         received.count)
                     )  %>%
                     addPolygons(
                       data = states_shapefiles,
                       color = "#000000",
                       weight = 1,
                       opacity = 1,
                       fillOpacity = 0,
                       options = pathOptions(clickable = FALSE)) %>%
                     fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
                     addLegend(
                       position = 'bottomright',
                       pal = us_palette,
                       values = choropleth_sf_tally[[the_fillColor]],
                       title = the_title_legend,
                       layerId = "legend"
                     )
                 }
                 
               )
             })

output$us_states_choropleth <- renderLeaflet({
  if (is.null(input$choropleth_checkbox_datefilter)) {
    return()
  }
  
  if (!input$choropleth_checkbox_datefilter) {
    if (is.null(input$choropleth_date_slider)) {
      return()
    }
  }
  
  the_fillColor <- switch(
    input$choropleth_how_tally,
    "sender" = "send.count",
    "receiver" = "received.count",
    "both" = "total.count"
  )
  
  the_title_legend <- switch(
    input$choropleth_how_tally,
    "sender" = "Letters Sent",
    "receiver" = "Letters Received",
    "both" = "Letters Sent & Received"
  )

  
  leaflet() %>%
    fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
    addLegend(labels = c("a","b"),
              colors = c("red", "blue"),
              layer = "legend")

  
})


# output$us_states_choropleth <- renderLeaflet({
#   if (is.null(input$choropleth_checkbox_datefilter)) {
#     return()
#   }
#   
#   if (!input$choropleth_checkbox_datefilter) {
#     if (is.null(input$choropleth_date_slider)) {
#       return()
#     }
#   }
#   
#   if (is.null(choropleth_sf_tally())) {
#     shinyjs::show(id = "loading-choropleth",
#                   anim = TRUE,
#                   animType = "fade")
#   } else {
#     shinyjs::hide(id = "loading-choropleth",
#                   anim = TRUE,
#                   animType = "fade")
#   }
#   
#   us_palette <- colorBin(
#     c("#cccccc", brewer.pal(5, "YlGnBu")),
#     bins = c(0, 1, 5, 10, 20, 50, 350),
#     pretty = FALSE
#   )
#   
# 
#   choropleth_sf_tally <- choropleth_sf_tally()
#   
#   bounds <-
#     c(-125, 24 , -75, 45) # http://rpubs.com/bhaskarvk/proj4leaflet
#   
#   the_fillColor <- switch(
#     input$choropleth_how_tally,
#     "sender" = "send.count",
#     "receiver" = "received.count",
#     "both" = "total.count"
#   )
# 
#   the_title_legend <- switch(
#     input$choropleth_how_tally,
#     "sender" = "Letters Sent",
#     "receiver" = "Letters Received",
#     "both" = "Letters Sent & Received"
#   )
#   
#   switch(
#     input$choropleth_boundaries_to_show,
#     "states" = {
#       choropleth_sf_tally %>%
#       leaflet() %>%
#         addPolygons(
#           fillColor = as.formula(paste0("~us_palette(",the_fillColor, ")")),
#           stroke = TRUE,
#           color = "#ffffff",
#           smoothFactor = 0.2,
#           fillOpacity = 0.8,
#           weight = 1,
#           popup = ~paste0(state.name,
#                           "</br>",
#                           "Letter sent: ",
#                           send.count,
#                           "</br>",
#                           "Letter received: ",
#                           received.count)
#         ) %>%
#         addLegend(
#           position = 'bottomright',
#           pal = us_palette,
#           values = as.formula(paste0("~", the_fillColor)),
#           title = the_title_legend
#         )
#     },
#     "congressional districts" = {
#       choropleth_sf_tally %>%
#       leaflet() %>%
#         addPolygons(
#           fillColor = as.formula(paste0("~us_palette(",the_fillColor, ")")),
#           stroke = TRUE,
#           color = "#ffffff",
#           smoothFactor = 0.2,
#           fillOpacity = 0.8,
#           weight = 1,
#           popup = ~paste0(
#             district.name,
#             "</br>",
#             state.name,
#             "</br>",
#             "Letter sent: ",
#             send.count,
#             "</br>",
#             "Letter received: ",
#             received.count)
#         ) %>%
#         addLegend(
#           position = 'bottomright',
#           pal = us_palette,
#           values = as.formula(paste0("~", the_fillColor)),
#           title = the_title_legend
#         )
#     },
#     "counties" = {
#       choropleth_sf_tally %>%
#       leaflet() %>%
#         addPolygons(
#           fillColor = as.formula(paste0("~us_palette(",the_fillColor, ")")),
#           stroke = TRUE,
#           color = "#ffffff",
#           smoothFactor = 0.2,
#           fillOpacity = 0.8,
#           weight = 1,
#           popup = ~paste0(
#             county.name,
#             "</br>",
#             state.name,
#             "</br>",
#             "Letter sent: ",
#             send.count,
#             "</br>",
#             "Letter received: ",
#             received.count)
#         ) %>%
#         addLegend(
#           position = 'bottomright',
#           pal = us_palette,
#           values = as.formula(paste0("~", the_fillColor)),
#           title = the_title_legend
#         )
#     }
#     
#   )
# 
#   
# })


