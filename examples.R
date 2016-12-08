## === Make spdf
send_locations_spdf <- letters_df %>% spdf_letters(send.or.receive = "sender")

both_locations_spdf <- letters_df %>% spdf_letters(send.or.receive = "both")

send_local_known <- letters_df %>%
  filter(!is.na(sender.latitude))

receive_local_known <- letters_df %>%
  filter(!is.na(receiver.latitude))

both_local_known <- letters_df %>%
  filter(!is.na(receiver.latitude) &
           !is.na(sender.location))

send_spdf <- send_local_known %>% spdf_letters(send.or.receive = "sender")
receive_spdf <- receive_local_known %>% spdf_letters(send.or.receive = "receiver")
both_spdf <- both_local_known %>% spdf_letters(send.or.receive = "both")

## === Count letters in states (returns SPDF)

send_local_known %>% spdf_letters(send.or.receive = "sender") %>%
  count_letters_in_states()

## === Choropleth

send_state_tallies_spdf <- send_local_known %>% spdf_letters(send.or.receive = "sender") %>%
  count_letters_in_states()

letters_df %>%
  filter(!is.na(sender.latitude)) %>%
  spdf_letters(send.or.receive = "sender") %>%
  count_letters_in_states() %>%
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
  )

## Journeys

journey_termini_data(both_local_known, send.or.receive = "sender")

my_journeys <- letter_journey_lines(both_local_known)

my_journeys$number.of.letters

label_journey <- function(sender.location = NA, receiver.location = NA, number.of.letters = NA){
  paste0(
    "<p>Sender: ", sender.location,
    "</p>",
    "<p>Receiver: ", receiver.location,
    "</p>",
    "<p>Number of letters: ", as.character(number.of.letters),
    "</p>"
  )
}

leaflet() %>%
  addTiles() %>%
  addPolylines(data = letter_journey_lines(both_local_known), color = "#2c7bb6", 
               popup = ~ label_journey(sender.location, receiver.location, number.of.letters),
               weight = 4) %>%
  addCircleMarkers(data = journey_termini_data(both_local_known, send.or.receive = "sender"), 
                   lng = ~sender.longitude, 
                   lat = ~sender.latitude,
                   fill = FALSE,
                   radius = 1.8,
                   stroke = TRUE,
                   color = "#fdae61",
                   opacity = 0.6,
                   popup = ~sender.location) %>%
  addCircleMarkers(data = journey_termini_data(both_local_known, send.or.receive = "receiver"), 
                   lng = ~receiver.longitude, 
                   lat = ~receiver.latitude,
                   fill = TRUE,
                   radius = 1.8,
                   stroke = TRUE,
                   color = "#d7191c",
                   opacity = 0.6,
                   popup = ~receiver.location)