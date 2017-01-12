locations_df <- read_csv("data/all_locations.csv")

letters_df <- read_csv("data/all_letters.csv")

letters_df <- letters_df %>%
  mutate(journey = paste(sender.latitude, sender.longitude, receiver.latitude, receiver.longitude))

usefulcols_letters_df <- c("sender.location",
                           "sender.latitude",
                           "sender.longitude",
                           "receiver.location",
                           "receiver.latitude",
                           "receiver.longitude",
                           "date",
                           "journey")

uselesscols_letters_df <- c("id.letter", "bytes", "na.ger", "letter.series", "relationship.sender.receiver", 
                            "social.strata.sender", "social.strata.receiver", "sender.latlong.string", 
                            "receiver.latlong.string", "tzone", "sender.country", "receiver.country", 
                            "data.ambiguous", "sender", "receiver", "location.sender", "location.receiver"
)


## ============== Shapefiles 
states_shapefiles <- readOGR(
  dsn = "data/shapefiles/",
  layer = "contiguous_states",
  verbose = F
)

counties_shapefiles <- readOGR(
  dsn = "data/shapefiles/",
  layer = "contiguous_counties",
  verbose = F
)

congressional_districts_shapefiles <- readOGR(
  dsn = "data/shapefiles/",
  layer = "contiguous_congressional_districts",
  verbose = F
)

proj4_string <- states_shapefiles@proj4string






