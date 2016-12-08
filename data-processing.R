locations_df <- read_csv("data/all_locations.csv")

letters_df <- read_csv("data/all_letters.csv")

states_shapefiles <- readOGR(
  dsn = "data/shapefiles/",
  layer = "contiguous_states",
  verbose = F
)