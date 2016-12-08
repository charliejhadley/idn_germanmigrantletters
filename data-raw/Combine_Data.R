library(lubridate)
library(stringr)
library(tidyverse)


## ====================================== First Set Locations
## ======================================

# This data is processed using an old script created as part of a Live Data case study.

# Import csv file
letters_maxqda_import <-
  read_csv(file = "data-raw/first_set/latters-MaxQDA-Format.csv")

### ======= Location df
locations_vec <-
  unique(
    c(
      letters_maxqda_import$Sender.Location,
      letters_maxqda_import$Receiver.Location
    )
  )

### For historic reasons, rename as entries_with_locations (Academic wants letters without a receive location to be included)

letters_maxqda_import <- letters_maxqda_import %>%
  filter(
    !is.na(Sender.Location.GIS.Latitude) &
      !is.na(Sender.Location.GIS.Longitude) &
      !is.na(Receiver.Location.GIS.Latitude) &
      !is.na(Receiver.Location.GIS.Longitude)
  )

letters_maxqda_import <- letters_maxqda_import %>%
  mutate(
    Sender.LatLong.String = paste(Sender.Location.GIS.Latitude, Sender.Location.GIS.Longitude),
    Receiver.LatLong.String = paste(
      Receiver.Location.GIS.Latitude,
      Receiver.Location.GIS.Longitude
    )
  ) %>%
  filter(Sender.LatLong.String != Receiver.LatLong.String) %>% # these letters are nonsensical
  mutate(Date = force_tz(dmy(Date, quiet = TRUE), tzone = "GMT"), tzone = "GMT") %>%
  filter(Date < as.POSIXct("2000/01/01")) # There's a date after 2000 that needs to be removed


## Filter by geographic element:
get.country <- function(location_string) {
  if (location_string == "" | location_string == "Schiff Sorrento") {
    "NA"
  } else {
    # strsplit(location_string, ",")[1]
    sapply(strsplit(location_string, ","), "[[", 1)
  }
}
letters_maxqda_import$Sender.Country <-
  unlist(lapply(letters_maxqda_import$Sender.Location, function(x)
    get.country(x)))
letters_maxqda_import$Receiver.Country <-
  unlist(lapply(letters_maxqda_import$Receiver.Location, function(x)
    get.country(x)))

firstset_letters <- letters_maxqda_import
colnames(firstset_letters) <-
  tolower(make.names(colnames(firstset_letters)))


firstset_letters <- firstset_letters %>%
  rename(
    sender.latitude = sender.location.gis.latitude,
    sender.longitude = sender.location.gis.longitude,
    receiver.latitude = receiver.location.gis.latitude,
    receiver.longitude = receiver.location.gis.longitude
  ) %>%
  distinct()


firstset_sender <-
  firstset_letters[, grepl("sender", colnames(firstset_letters))]

firstset_sender <- firstset_sender %>%
  rename(
    location.string = sender.location,
    latitude = sender.latitude,
    longitude = sender.longitude,
    latlong.string = sender.latlong.string,
    country = sender.country
  )

firstset_receiver <-
  firstset_letters[, grepl("receiv", colnames(firstset_letters))]

firstset_receiver <- firstset_receiver %>%
  rename(
    location.string = receiver.location,
    latitude = receiver.latitude,
    longitude = receiver.longitude,
    latlong.string = receiver.latlong.string,
    country = receiver.country
  )

firstset_locations <-
  full_join(firstset_receiver, firstset_sender) %>%
  select(-relationship.sender.receiver,-social.strata.receiver,-social.strata.sender) %>%
  unique()

## ====================================== Second Set Locations
## ======================================

secondset_letters <-
  read_csv("data-raw/second_set/secondset_letters.csv")
colnames(secondset_letters) <-
  tolower(make.names(colnames(secondset_letters)))

secondset_letters <- secondset_letters %>%
  mutate(date = dmy(date))



secondset_locations <-
  read_csv("data-raw/second_set/secondset_locations.csv")
colnames(secondset_locations) <-
  tolower(make.names(colnames(secondset_locations)))
secondset_locations <- secondset_locations %>%
  select(-x9, -x10, -x11, -x12, -x13) %>%
  filter(!is.na(longitude))


## Filter out locations not mentioned in the letters
secondset_locations <- secondset_locations %>%
  filter(location.string %in% unique(
    c(
      secondset_letters$location.sender,
      secondset_letters$location.receiver
    )
  )) %>%
  select(-city.with.brackets, -brackets, -ambiguous) # These don't exist in the initial set, just throw away

str(firstset_letters)


## ====================================== Combining Letters
## ======================================

combined_letters <- firstset_letters %>%
  rename(id.letter = document.name) %>%
  select(-x1) %>%
  full_join(secondset_letters)

## Fix bad encodings
combined_letters$location.sender <-
  gsub("\xa7", "ß", combined_letters$location.sender) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) %>% # \x8a is used instead of ä
  gsub("\x9a", "ö", .) %>%
  gsub("\x9f", "ü", .) %>%
  gsub("'", "", .) %>%
  gsub("\x8e", "é", .)

combined_letters$location.receiver <-
  gsub("\xa7", "ß", combined_letters$location.receiver) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) %>% # \x8a is used instead of ä
  gsub("\x9a", "ö", .) %>%
  gsub("\x9f", "ü", .) %>%
  gsub("'", "", .) %>%
  gsub("\x8e", "é", .)

write_csv(combined_letters, "data/all_letters.csv")

## ====================================== Combining Locations
## ======================================


combined_locations <-
  full_join(firstset_locations, secondset_locations) %>%
  distinct()

## Fix bad encodings
combined_locations$country <-
  gsub("\xa7", "ß", combined_locations$country) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) # \x8a is used instead of ä

combined_locations$city <-
  gsub("\xa7", "ß", combined_locations$city) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) %>% # \x8a is used instead of ä
  gsub("\x9a", "ö", .) %>%
  gsub("\x9f", "ü", .) %>%
  gsub("'", "", .) %>%
  gsub("\x8e", "é", .)

write_csv(combined_locations, "data/all_locations.csv")
