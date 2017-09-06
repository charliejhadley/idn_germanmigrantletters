library("tidyverse")
library("readxl")
library("lubridate")

letter_series <- read_csv("data-raw/28thJuly/DatasetPublic.csv")
colnames(letter_series) <- tolower(colnames(letter_series))

letter_series <- letter_series %>%
  mutate(date = dmy(date))

letter_locations <- read_excel("data-raw/28thJuly/Letters unique locations with GIS data.xlsx")
colnames(letter_locations) <- tolower(make.names(colnames(letter_locations)))

letter_locations <- letter_locations %>%
  filter(grepl(",", location.string)) %>%
  separate(location.string, into = c("country", "other"), remove = FALSE,extra = "merge") %>%
  select(location.string, latitude, longitude, country) %>%
  unique() %>%
  mutate(location.id = row_number() - 1)


letter_series <- letter_series %>%
  rename(id.letter = id,
         sender.name = sender,
         receiver.name = receiver, 
         sender.location = location.sender,
         receiver.location = location.receiver) %>%
  select(-x1)

letter_series <- letter_series %>%
  mutate(sender.latitude = plyr::mapvalues(sender.location,
                                           from = letter_locations$location.string,
                                           to = letter_locations$latitude,
                                           warn_missing = FALSE),
         sender.longitude = plyr::mapvalues(sender.location,
                                           from = letter_locations$location.string,
                                           to = letter_locations$longitude,
                                           warn_missing = FALSE),
         receiver.latitude = plyr::mapvalues(receiver.location,
                                           from = letter_locations$location.string,
                                           to = letter_locations$latitude,
                                           warn_missing = FALSE),
         receiver.longitude = plyr::mapvalues(receiver.location,
                                             from = letter_locations$location.string,
                                             to = letter_locations$longitude,
                                             warn_missing = FALSE))

letter_series <- letter_series %>%
  mutate(sender.latitude = as.numeric(sender.latitude),
         sender.longitude = as.numeric(sender.longitude),
         receiver.latitude = as.numeric(receiver.latitude),
         receiver.longitude = as.numeric(receiver.longitude))


backup_letter_series <- read_csv("data/all_letters_backup.csv")



colnames(backup_letter_series)
colnames(letter_series)

letter_series %>%
  write_csv("data/all_letters.csv")




