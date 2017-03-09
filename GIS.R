
## ============== Convert df to SPDF

spdf_letters <- function(letters.data = NA, send.or.receive = "both"){
  switch(send.or.receive,
         "both" = {

           send_letters <- letters.data %>%
             select_("sender.latitude", "sender.longitude") %>%
             rename_("latitude" = "sender.latitude", "longitude" = "sender.longitude")
           
           receive_letters <- letters.data %>%
             select_("receiver.latitude", "receiver.longitude") %>%
             rename_("latitude" = "receiver.latitude", "longitude" = "receiver.longitude")

           letters_data <- rbind(send_letters,receive_letters) %>%
             select_("longitude", "latitude") %>% # SPDF are longitude, latitude pairs
             na.omit() %>%
             SpatialPointsDataFrame(
               coords = .,
               data = .,
               proj4string = proj4_string
             )
         },
         "sender" = {
           
           send_letters <- letters.data %>%
             select_("sender.latitude", "sender.longitude") %>%
             rename_("latitude" = "sender.latitude", "longitude" = "sender.longitude")
           
           send_letters %>%
             select_("longitude", "latitude") %>% # SPDF are longitude, latitude pairs
             na.omit() %>%
             SpatialPointsDataFrame(
               coords = .,
               data = .,
               proj4string = proj4_string
             )
           
         },
         "receiver" = {
           
           receive_letters <- letters.data %>%
             select_("receiver.latitude", "receiver.longitude") %>%
             rename_("latitude" = "receiver.latitude", "longitude" = "receiver.longitude")
           
           receive_letters %>%
             select_("longitude", "latitude") %>% # SPDF are longitude, latitude pairs
             na.omit() %>%
             SpatialPointsDataFrame(
               coords = .,
               data = .,
               proj4string = proj4_string
             )
         })
}



## ============== Count letters per state

count_letters_in_states <- function(letters.spdf = NA){
  shapefiles <- states_shapefiles
  
  contiguous_counts <-
    poly.counts(pts = letters.spdf, polys = shapefiles)
  contiguous_counts
  
  contiguous_counts_df <- data.frame(contiguous_counts)
  shapefiles@data$Count.of.Send.Locations <-
    contiguous_counts_df$contiguous_counts
  # Return for use
  polygons_with_tallies <- shapefiles
  polygons_with_tallies
}

count_letters_in_regions <- function(letters.spdf, shape.files){
  shapefiles <- shape.files
  
  contiguous_counts <-
    poly.counts(pts = letters.spdf, polys = shapefiles)

  
  contiguous_counts_df <- data.frame(contiguous_counts)

  shapefiles@data$Count.of.Send.Locations <-
    contiguous_counts_df$contiguous_counts
  # Return for use
  polygons_with_tallies <- shapefiles
  polygons_with_tallies
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
  print("pass")
  letter_journeys <- gcIntermediate(
    tallied_letters %>%
      select_("sender.longitude", "sender.latitude"),
    tallied_letters %>%
      select_("receiver.longitude", "receiver.latitude"),
    sp = TRUE,
    addStartEnd = TRUE
  )
  
  letter_journeys$number.of.letters <- tallied_letters$number.of.letters
  letter_journeys$sender.location <- tallied_letters$sender.location
  letter_journeys$receiver.location <- tallied_letters$receiver.location
  # Return for use
  letter_journeys
}

journey_termini_data <- function(letters.data = NA, send.or.receive = NA){
  switch(send.or.receive,
         "sender" = {
           letters.data %>%
             select(-contains("receiver")) %>% # remove receiver cols
             select(-one_of(uselesscols_letters_df), -date) %>% # remove unique cols
             distinct() %>%
             group_by(sender.location) %>%
             mutate(total.sent = n()) %>%
             ungroup()
         },
         "receiver" = {
           letters.data %>%
             select(-contains("sender")) %>% # remove receiver cols
             select(-one_of(uselesscols_letters_df), -date) %>% # remove unique cols
             distinct() %>%
             group_by(receiver.location) %>%
             mutate(total.received = n()) %>%
             ungroup()
         })
}

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

label_termini_sender <- function(sender.location, total.sent){
  paste0(
    "<p>Send Location: ", sender.location,
    "</p>",
    "<p>Number of letters sent: ", total.sent,
    "</p>"
  )
}

label_termini_receiver <- function(receiver.location, total.received){
  paste0(
    "<p>Send Location: ", receiver.location,
    "</p>",
    "<p>Number of letters received: ", total.received,
    "</p>"
  )
}



