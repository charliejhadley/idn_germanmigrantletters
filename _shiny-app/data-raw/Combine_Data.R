library(plyr)
library(tidyverse)
library(lubridate)

## ====================================== First Set Locations
## ======================================

# This data is processed using an old script created as part of a Live Data case study.

# Import csv file
firstset_letters <-
  read_csv(file = "data-raw/first_set/latters-MaxQDA-Format.csv")
colnames(firstset_letters) <- tolower(colnames(firstset_letters))

## Tidy names
firstset_letters <- firstset_letters %>%
  rename(id.letter = document.name,
         sender.latitude = sender.location.gis.latitude,
         sender.longitude = sender.location.gis.longitude,
         receiver.latitude = receiver.location.gis.latitude,
         receiver.longitude = receiver.location.gis.longitude) %>%
  filter(!is.na(sender.latitude),
         !is.na(receiver.latitude)) %>%
  select(-x1, -bytes, -na.ger, -social.strata.receiver, -social.strata.sender, -relationship.sender.receiver)

## Remove letters where send == receiver and add dates
firstset_letters <- firstset_letters %>%
  filter(!paste(sender.latitude, sender.longitude) == paste(receiver.latitude, receiver.longitude)) %>%
  mutate(date = force_tz(dmy(date, quiet = TRUE), tzone = "GMT"), tzone = "GMT") %>%
  filter(date < as.POSIXct("2000/01/01")) # There's a date after 2000 that needs to be removed

## Filter by geographic element:
get.country <- function(location_string) {
  if (location_string == "" | location_string == "Schiff Sorrento") {
    "NA"
  } else {
    # strsplit(location_string, ",")[1]
    sapply(strsplit(location_string, ","), "[[", 1)
  }
}
firstset_letters$sender.country <-
  unlist(lapply(firstset_letters$sender.location, function(x)
    get.country(x)))
firstset_letters$receiver.country <-
  unlist(lapply(firstset_letters$receiver.location, function(x)
    get.country(x)))

## ====================================== First Set Locations
## ======================================

firstset_sender <-
  firstset_letters[, grepl("sender", colnames(firstset_letters))]

firstset_sender <- firstset_sender %>%
  rename(
    location.string = sender.location,
    latitude = sender.latitude,
    longitude = sender.longitude,
    country = sender.country
  )

firstset_receiver <-
  firstset_letters[, grepl("receiv", colnames(firstset_letters))]

firstset_receiver <- firstset_receiver %>%
  rename(
    location.string = receiver.location,
    latitude = receiver.latitude,
    longitude = receiver.longitude,
    country = receiver.country
  )

firstset_locations <-
  full_join(firstset_receiver, firstset_sender) %>%
  unique()

## ====================================== Second Set Letters
## ======================================

secondset_letters <-
  read_csv("data-raw/second_set/secondset_letters.csv")
colnames(secondset_letters) <-
  tolower(make.names(colnames(secondset_letters)))

## Tidy, format and drop columns
secondset_letters <- secondset_letters %>%
  mutate(date = dmy(date)) %>%
  rename(sender.location = location.sender,
         receiver.location = location.receiver) %>%
  select(-data.ambiguous,
         -sender,
         -receiver) %>%
  filter(!is.na(sender.location),
         !is.na(receiver.location))

## Fix bad encodings
secondset_letters$sender.location <-
  gsub("\xa7", "ß", secondset_letters$sender.location) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) %>% # \x8a is used instead of ä
  gsub("\x9a", "ö", .) %>%
  gsub("\x9f", "ü", .) %>%
  gsub("'", "", .) %>%
  gsub("\x8e", "é", .)

secondset_letters$receiver.location <-
  gsub("\xa7", "ß", secondset_letters$receiver.location) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) %>% # \x8a is used instead of ä
  gsub("\x9a", "ö", .) %>%
  gsub("\x9f", "ü", .) %>%
  gsub("'", "", .) %>%
  gsub("\x8e", "é", .)

## Add country columns
secondset_letters$sender.country <-
  unlist(lapply(secondset_letters$sender.location, function(x)
    get.country(x)))

secondset_letters$receiver.country <-
  unlist(lapply(secondset_letters$receiver.location, function(x)
    get.country(x)))


## ====================================== Second Set Locations
## ======================================

secondset_locations <-
  read_csv("data-raw/second_set/secondset_locations.csv")
colnames(secondset_locations) <-
  tolower(make.names(colnames(secondset_locations)))
secondset_locations <- secondset_locations %>%
  select(-x9, -x10, -x11, -x12, -x13) %>%
  filter(!is.na(longitude)) %>%
  select(-city.with.brackets, -brackets, -ambiguous, -city)

# ## Filter out locations not mentioned in the letters
# secondset_locations <- secondset_locations %>%
#   filter(location.string %in% unique(secondset_letters$sender.location) &
#            location.string %in% unique(secondset_letters$receiver.location))


foobar <- secondset_locations %>%
  filter(location.string %in% unique(secondset_letters$sender.location) &
           location.string %in% unique(secondset_letters$receiver.location)) %>%
  select(-city.with.brackets, -brackets, -ambiguous, -city)




colnames(secondset_locations)
colnames(firstset_locations)

## ====================================== Combining Locations
## ======================================


combined_locations <-
  full_join(firstset_locations, secondset_locations) %>%
  distinct()

write_csv(combined_locations, "data/all_locations.csv")

## ====================================== Add locations to secondset_letters
## ======================================

## Drop letters with locations not given in combined_locations
secondset_letters <- secondset_letters %>%
  filter(sender.location %in% combined_locations$location.string &
           receiver.location %in% combined_locations$location.string)

sec_set_sendlocs <- secondset_letters %>%
  select(sender.location) %>% .[[1]]

## Add sender coordinates/


mapvalues(unique(sec_set_sendlocs),
from = combined_locations$location.string,
to = paste(combined_locations$latitude,
           combined_locations$longitude), warn_missing = FALSE)


secondset_letters <- secondset_letters %>%
  mutate(sender.latlong.string = mapvalues(sender.location,
                                           from = combined_locations$location.string,
                                           to = paste(combined_locations$latitude,
                                                      combined_locations$longitude))) %>%
  mutate(receiver.latlong.string = mapvalues(receiver.location,
                                           from = combined_locations$location.string,
                                           to = paste(combined_locations$latitude,
                                                      combined_locations$longitude))) %>%
  separate(sender.latlong.string, c("sender.latitude", "sender.longitude"), " ") %>%
  separate(receiver.latlong.string, c("receiver.latitude", "receiver.longitude"), " ") %>%
  mutate(sender.latitude = as.numeric(sender.latitude),
         sender.longitude = as.numeric(sender.longitude),
         receiver.latitude = as.numeric(receiver.latitude),
         receiver.longitude = as.numeric(receiver.longitude))

## ====================================== Combining Letters
## ======================================

combined_letters <- firstset_letters %>%
  select(one_of(intersect(colnames(firstset_letters),
                          colnames(secondset_letters)))) %>%
  bind_rows(secondset_letters %>%
              select(one_of(intersect(colnames(firstset_letters),
                                      colnames(secondset_letters)))))

## Fix bad encodings
combined_letters$sender.location <-
  gsub("\xa7", "ß", combined_letters$sender.location) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) %>% # \x8a is used instead of ä
  gsub("\x9a", "ö", .) %>%
  gsub("\x9f", "ü", .) %>%
  gsub("'", "", .) %>%
  gsub("\x8e", "é", .)

combined_letters$receiver.location <-
  gsub("\xa7", "ß", combined_letters$receiver.location) %>% # \xa7 is used instead of ß
  gsub("\x8a", "ä", .) %>% # \x8a is used instead of ä
  gsub("\x9a", "ö", .) %>%
  gsub("\x9f", "ü", .) %>%
  gsub("'", "", .) %>%
  gsub("\x8e", "é", .)

write_csv(combined_letters, "data/all_letters.csv")


