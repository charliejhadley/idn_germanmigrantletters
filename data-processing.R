locations_df <- read_csv("data/all_locations.csv") %>%
  unique() %>%
  mutate(location.id = row_number() - 1)


letters_df <- read_csv("data/all_letters.csv")

letters_df <- letters_df %>%
  mutate(journey = paste(sender.latitude, sender.longitude, receiver.latitude, receiver.longitude))

letters_df <- letters_df %>%
  separate(sender.location, into = c("sender.country", "sender.city"), extra = "merge", remove = FALSE) %>%
  mutate(sender.state = str_extract(sender.city, "\\([^()]+\\)")) %>%
  mutate(sender.state = gsub("\\(|\\)", "", sender.state)) %>%
  filter(str_length(sender.state) <= 2) %>%
  mutate(sender.city = trimws(str_replace(sender.city, "\\([^()]{0,}\\)", ""))) %>%
  separate(receiver.location, into = c("receiver.country", "receiver.city"), extra = "merge", remove = FALSE) %>%
  mutate(receiver.state = str_extract(receiver.city, "\\([^()]+\\)")) %>%
  mutate(receiver.state = gsub("\\(|\\)", "", receiver.state)) %>%
  mutate(receiver.city = trimws(str_replace(receiver.city, "\\([^()]{0,}\\)", "")))

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

## ============== Add decade column

earliest_decade <- year(floor_date(min(letters_df$date, na.rm = TRUE), unit = "10 years"))

latest_decade <- year(ceiling_date(max(letters_df$date, na.rm = TRUE), unit = "10 years"))

letters_df <- letters_df %>%
  mutate(decade = cut(year(date),
                      breaks = seq(earliest_decade, latest_decade, by = 10),
                      right = FALSE,
                      dig.lab = 4)) %>%
  mutate(decade = fct_recode(
    decade,
    "1810-1819" = "[1810,1820)",
    "1820-1829" = "[1820,1830)",
    "1830-1839" = "[1830,1840)",
    "1840-1849" = "[1840,1850)",
    "1850-1859" = "[1850,1860)",
    "1860-1869" = "[1860,1870)",
    "1870-1879" = "[1870,1880)",
    "1880-1889" = "[1880,1890)",
    "1890-1899" = "[1890,1900)",
    "1900-1909" = "[1900,1910)",
    "1910-1919" = "[1910,1920)",
    "1920-1929" = "[1920,1930)",
    "1930-1939" = "[1930,1940)",
    "1940-1949" = "[1940,1950)",
    "1950-1959" = "[1950,1960)",
    "1960-1969" = "[1960,1970)",
    "1970-1979" = "[1970,1980)"
  ))


## ============== Shapefiles 

# load("data/states_shapefiles.rdata")
# proj4_string <- states_shapefiles@proj4string
# load("data/counties_shapefiles.rdata")
# load("data/congressional_districts_shapefiles.rdata")

states_shapefiles <- shp_all_us_states %>%
  filter(contiguous.united.states)
counties_shapefiles <- shp_all_us_counties %>%
  filter(contiguous.united.states)
congressional_districts_shapefiles <- shp_all_us_congressional_districts %>%
  filter(contiguous.united.states)




### ========== alberusa
# 
# library(albersusa)
# system.file("extdata/composite_us_states.geojson.gz", package="albersusa")
# 
# foo_geojson <- rgdal::readOGR("data/composite_us_counties.geojson")
# 
# foo_geojson$name

## ============== Selected families

selected_family_letter_series <- c("S001", "G004", "B117")

letters_df <-letters_df %>%
  mutate(selected.family = ifelse(grepl(paste0(selected_family_letter_series, collapse = "|"), id.letter),
         TRUE,FALSE))

selected_families_letters <- letters_df %>%
  filter(selected.family)

unique_selected_letter_locations <- selected_families_letters %>%
  select(sender.longitude, sender.latitude) %>%
  unique() %>%
  mutate(ggplot.necessity = "Send Location") %>%
  st_as_sf(coords = c("sender.longitude", "sender.latitude"), crs = st_crs(states_shapefiles))


