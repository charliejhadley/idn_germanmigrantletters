addLegendCustom <-
  function(map, colors, labels, sizes, opacity = 0.5, ...) {
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
      opacity = opacity,
      ...
    ))
  }

## ============== count_letters_in_shp

count_letters_in_shp <- function(letters.data = NA, shp) {
  
  if(nrow(letters.data) == 0){
    shp %>%
      mutate(
        total.count = 0,
        received.count = 0,
        send.count = 0
      )
  } else {
    send_letters <- letters.data %>%
      select(sender.latitude, sender.longitude) %>%
      rename(latitude = sender.latitude, longitude = sender.longitude) %>%
      na.omit()
    
    receive_letters <- letters.data %>%
      select(receiver.latitude, receiver.longitude) %>%
      rename(latitude = receiver.latitude, longitude = receiver.longitude) %>%
      na.omit()
    
    total_letters <- send_letters %>%
      bind_rows(receive_letters) %>%
      # unique() %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = st_crs(shp))
    
    sent_letters <- send_letters %>%
      # unique() %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = st_crs(shp))
    
    received_letters <- receive_letters %>%
      # unique() %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = st_crs(shp))
    
    the_total.count <- lengths(st_covers(shp, total_letters))
    the_received.count <- lengths(st_covers(shp, received_letters))
    the_send.count <- lengths(st_covers(shp, sent_letters))
    
    
    shp %>%
      mutate(
        total.count = the_total.count,
        received.count = the_received.count,
        send.count = the_send.count
      )
    
  }
  
  
}

## ============== Empty states

state_outline_only <- {
  shapefiles <- states_shapefiles
}

## ============== Great circles
## Refer to http://personal.tcu.edu/kylewalker/interactive-flow-visualization-in-r.html

letter_journey_lines <- function(letters.data, unique.or.all = "unique"){
  
  
  unique.or.all <- rlang::arg_match(unique.or.all,  c("unique", "all"))
  
  unique_journies <- letters.data %>%
    filter(!{is.na(sender.latitude) | is.na(receiver.latitude)}) %>%
    group_by(journey) %>%
    mutate(number.of.letters = n()) %>%
    ungroup() %>%
    select(contains("sender"),
           contains("receiver"),
           number.of.letters,
           -contains("name"),
           selected.family,
           date,
           decade) %>%
    unique()
  
  
  if(nrow(unique_journies) == 0){
    tibble(
      lat = as.numeric(),
      lng = as.numeric()
    )
  } else {
    send_unique_journies <- unique_journies %>%
      select(sender.longitude, sender.latitude) %>%
      rename(
        longitude = sender.longitude,
        latitude = sender.latitude) %>%
      mutate(journey.id = row_number())
    
    receive_unique_journies <- unique_journies %>%
      select(receiver.longitude, receiver.latitude) %>%
      rename(
        longitude = receiver.longitude,
        latitude = receiver.latitude) %>%
      mutate(journey.id = row_number())
    
    df_with_linestrings <- send_unique_journies %>%
      bind_rows(receive_unique_journies) %>%
      sf::st_as_sf(coords = c("longitude","latitude")) %>%
      group_by(journey.id) %>%
      arrange(journey.id) %>%
      summarise() %>%
      sf::st_cast("LINESTRING")
    
    st_geometry(unique_journies) <- st_geometry(df_with_linestrings)
    
    unique_journies <- st_set_crs(unique_journies, st_crs(shp_all_us_states))
    
    if(unique.or.all == "unique"){
      unique_journies <- unique_journies %>%
        select(-date, -decade) %>%
        unique()
    }
    
    unique_journies %>%
      st_segmentize(units::set_units(500, km))
  }
  
  
}

## letters bounding box

bbox_letter_journeys <- letter_journey_lines(letters_df) %>%
  st_geometry() %>%
  attributes() %>%
  .$bbox %>%
  as.list()

my_fitBounds <- function(map, bbox){
  fitBounds(map, bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)
}


journey_termini_data <- function(letters.data) {
  receive_points <- letters.data %>%
    group_by(location.receiver) %>%
    mutate(total.received = n()) %>%
    ungroup() %>%
    select(contains("receiv")) %>%
    unique() %>%
    rename(
      location.name = location.receiver,
      latitude = receiver.latitude,
      longitude = receiver.longitude,
      country = receiver.country
    )
  
  send_points <- letters.data %>%
    group_by(location.sender) %>%
    mutate(total.sent = n()) %>%
    ungroup() %>%
    select(contains("sen")) %>%
    unique() %>%
    rename(
      location.name = location.sender,
      latitude = sender.latitude,
      longitude = sender.longitude,
      country = sender.country
    )
  
  full_join(receive_points, send_points) %>%
    select(location.name,
           latitude,
           longitude,
           country,
           total.sent,
           total.received)
}

label_journey <-
  function(location.sender = NA,
           location.receiver = NA,
           number.of.letters = NA) {
    paste0(
      "<p>Sender: ",
      location.sender,
      "</p>",
      "<p>Receiver: ",
      location.receiver,
      "</p>",
      "<p>Number of letters: ",
      as.character(number.of.letters),
      "</p>"
    )
  }

send_only_markers <- function(map, termini.data) {
  send.only.locs <- journey_termini_data(termini.data) %>%
    filter(total.sent > 0 & is.na(total.received)) %>%
    unique()
  
  addCircleMarkers(
    map,
    data = send.only.locs,
    lng = ~ longitude,
    lat = ~ latitude,
    fill = TRUE,
    radius = 1.8,
    stroke = TRUE,
    color = "#fdae61",
    popup = ~ paste0(
      "<p>Send Location: ",
      location.name,
      "</p>",
      "<p>Number of letters sent: ",
      total.sent,
      "</p>"
    ),
    opacity = 0.5
  )
}

receive_only_markers <- function(map, termini.data) {
  receive.only.locs <- journey_termini_data(termini.data) %>%
    filter(total.received > 0 & is.na(total.sent)) %>%
    unique()
  
  addCircleMarkers(
    map,
    data = receive.only.locs,
    lng = ~ longitude,
    lat = ~ latitude,
    fill = TRUE,
    radius = 1.8,
    stroke = TRUE,
    color = "#d7191c",
    popup = ~ paste0(
      "<p>Receive Location: ",
      location.name,
      "</p>",
      "<p>Number of letters received: ",
      total.received,
      "</p>"
    ),
    opacity = 0.5
  )
}

two_way_markers <- function(map, termini.data) {
  receive.only.locs <- journey_termini_data(termini.data) %>%
    filter(total.sent > 0 & total.received > 0) %>%
    unique()
  
  addCircleMarkers(
    map,
    data = receive.only.locs,
    lng = ~ longitude,
    lat = ~ latitude,
    fill = TRUE,
    radius = 1.8,
    stroke = TRUE,
    color = "#7570b3",
    popup = ~ paste0(
      "<p>Two-way Location: ",
      location.name,
      "</p>",
      "<p>Number of letters received: ",
      total.received,
      "</p>",
      "<p>Number of letters send: ",
      total.sent,
      "</p>"
    ),
    opacity = 0.5
  )
}
