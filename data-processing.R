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
proj4_string <- states_shapefiles@proj4string

colnames(states_shapefiles@data) <- tolower(colnames(states_shapefiles@data))

counties_shapefiles <- readOGR(
  "data/composite_us_counties.geojson",
  verbose = F
)
# 
# congressional_districts_shapefiles <- readOGR(
#   dsn = "data/shapefiles/",
#   layer = "contiguous_congressional_districts",
#   verbose = F
# )

congressional_districts_shapefiles <- readOGR(
  dsn = "data/shapefiles/",
  layer = "tl_2016_us_cd115",
  verbose = F
)
## Remove congressional districts at large
congressional_districts_shapefiles <- congressional_districts_shapefiles[!congressional_districts_shapefiles$CD115FP == "00",]
colnames(congressional_districts_shapefiles@data) <- tolower(colnames(congressional_districts_shapefiles@data))

colnames(congressional_districts_shapefiles@data) <- plyr::mapvalues(colnames(congressional_districts_shapefiles@data), from = "namelsad", to = "name")

congressional_districts_shapefiles <- contiguous_congressional_districts_spdf


### ========== alberusa
# 
# library(albersusa)
# system.file("extdata/composite_us_states.geojson.gz", package="albersusa")
# 
# foo_geojson <- rgdal::readOGR("data/composite_us_counties.geojson")
# 
# foo_geojson$name



