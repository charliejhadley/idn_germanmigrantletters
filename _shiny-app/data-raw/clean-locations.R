library("tidyverse")
library("readxl")


all_locations <- read_xlsx("data-raw/unique-locations.xlsx")

colnames(all_locations) <- tolower(make.names(colnames(all_locations)))


all_locations %>%
  mutate(new.location.string = location.string) %>%
  filter(grepl(",", location.string)) %>%
  separate(new.location.string, into = c("country", "location")) %>%
  select(-location) %>%
  filter(!is.na(latitude)) %>%
  write_csv("data/all_locations.csv")
