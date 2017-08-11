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

letter_journey_lines <- function(letters.data = NA) {
  ## Tally letters for polylines
  
  tallied_letters <- letters.data %>%
    group_by(journey) %>%
    mutate(number.of.letters = n()) %>%
    ungroup() %>%
    select_("-date") %>%
    unique()
  
  letter_journeys <- gcIntermediate(
    tallied_letters %>%
      select_("sender.longitude", "sender.latitude"),
    tallied_letters %>%
      select_("receiver.longitude", "receiver.latitude"),
    sp = TRUE,
    addStartEnd = TRUE
  )
  
  letter_journeys$number.of.letters <-
    tallied_letters$number.of.letters
  letter_journeys$sender.location <- tallied_letters$sender.location
  letter_journeys$receiver.location <-
    tallied_letters$receiver.location
  # Return for use
  letter_journeys
}

journey_termini_data <- function(letters.data) {
  receive_points <- letters.data %>%
    group_by(receiver.location) %>%
    mutate(total.received = n()) %>%
    ungroup() %>%
    select(contains("receiv")) %>%
    unique() %>%
    rename(
      location.name = receiver.location,
      latitude = receiver.latitude,
      longitude = receiver.longitude,
      country = receiver.country
    )
  
  send_points <- letters.data %>%
    group_by(sender.location) %>%
    mutate(total.sent = n()) %>%
    ungroup() %>%
    select(contains("sen")) %>%
    unique() %>%
    rename(
      location.name = sender.location,
      latitude = sender.latitude,
      longitude = sender.longitude,
      country = sender.country
    )
  
  full_join(receive_points, send_points)
}

label_journey <-
  function(sender.location = NA,
           receiver.location = NA,
           number.of.letters = NA) {
    paste0(
      "<p>Sender: ",
      sender.location,
      "</p>",
      "<p>Receiver: ",
      receiver.location,
      "</p>",
      "<p>Number of letters: ",
      as.character(number.of.letters),
      "</p>"
    )
  }

send_only_markers <- function(map, termini.data) {
  send.only.locs <- journey_termini_data(termini.data) %>%
    filter(total.sent > 0 & is.na(total.received))
  
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
    opacity = 0.6
  )
}

receive_only_markers <- function(map, termini.data) {
  receive.only.locs <- journey_termini_data(termini.data) %>%
    filter(total.received > 0 & is.na(total.sent))
  
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
    opacity = 0.6
  )
}

two_way_markers <- function(map, termini.data) {
  receive.only.locs <- journey_termini_data(termini.data) %>%
    filter(total.sent > 0 & total.received > 0)
  
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
    opacity = 0.8
  )
}
