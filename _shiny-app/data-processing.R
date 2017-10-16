## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: felix.krawatzek@nuffield.ox.ac.uk
## Data Source: https://doi.org/10.6084/m9.figshare.4516772
## ================================================================================

## ================ Obtain data from Figshare =================================
## ==============================================================================

fs_deposit_id <- 4516772
deposit_details <- fs_details(fs_deposit_id)

deposit_details <- unlist(deposit_details$files)
deposit_details <-
  data.frame(split(deposit_details, names(deposit_details)), stringsAsFactors = F)

deposit_details %>%
  filter(name == "Unique locations with GIS data.xlsx") %>%
  arrange(desc(size)) %>%
  select(download_url) %>%
  slice(1) %>%
  .[[1]] %>%
  download.file(destfile = "data/temp_figshare_all-locations.xlsx")

## ================ Import letter text =================================
## ==============================================================================

text_of_letters <- deposit_details %>%
  filter(name == "Letters-S001.csv") %>%
  select(download_url) %>%
  .[[1]] %>%
  read.csv(stringsAsFactors = FALSE, fileEncoding="latin1") %>%
  as_tibble()
colnames(text_of_letters) <- tolower(make.names(colnames(text_of_letters)))


## ================ Import Locations =================================
## ==============================================================================

locations_df <- read_xlsx("data/temp_figshare_all-locations.xlsx") %>%
  unique() %>%
  filter(!is.na(latitude)) %>%
  mutate(location.id = row_number() - 1) %>%
  setNames(., tolower(make.names(colnames(.))))


## ================ Combine letter details with locations =======================
## ==============================================================================
# 
letters_df <- deposit_details %>%
  filter(name == "DatasetPublic.csv") %>%
  select(download_url) %>%
  .[[1]] %>%
  read_csv(local = locale(encoding = "UTF-8")) %>%
  setNames(. , tolower(colnames(.))) %>%
  rename(id.letter = id)

# letters_df <- read_csv("data/manual-download_DatasetPublic.csv") %>%
#   setNames(. , tolower(colnames(.))) %>%
#   rename(id.letter = id)

## Tidy dates
letters_df <- letters_df %>%
  mutate(date = dmy(date))

## Combine locations
letters_df <- letters_df %>%
  left_join(locations_df, by = c("location.sender" = "location.string")) %>%
  select(-location.id) %>%
  rename(sender.latitude = latitude,
         sender.longitude = longitude) %>%
  left_join(locations_df, by = c("location.receiver" = "location.string")) %>%
  select(-location.id) %>%
  rename(receiver.latitude = latitude,
         receiver.longitude = longitude)

letters_df <- letters_df %>%
  mutate(journey = paste(sender.latitude, sender.longitude, receiver.latitude, receiver.longitude))

letters_df <- letters_df %>%
  separate(location.sender, into = c("sender.country", "sender.city"), extra = "merge", remove = FALSE) %>%
  mutate(sender.state = str_extract(sender.city, "\\([^()]+\\)")) %>%
  mutate(sender.state = gsub("\\(|\\)", "", sender.state)) %>%
  filter(str_length(sender.state) <= 2) %>%
  mutate(sender.city = trimws(str_replace(sender.city, "\\([^()]{0,}\\)", ""))) %>%
  separate(location.receiver, into = c("receiver.country", "receiver.city"), extra = "merge", remove = FALSE) %>%
  mutate(receiver.state = str_extract(receiver.city, "\\([^()]+\\)")) %>%
  mutate(receiver.state = gsub("\\(|\\)", "", receiver.state)) %>%
  mutate(receiver.city = trimws(str_replace(receiver.city, "\\([^()]{0,}\\)", "")))

## ================ Useful and not useful column names =======================
## ==============================================================================

usefulcols_letters_df <- c("location.sender",
                           "sender.latitude",
                           "sender.longitude",
                           "location.receiver",
                           "receiver.latitude",
                           "receiver.longitude",
                           "date",
                           "journey")

uselesscols_letters_df <- c("id.letter", "bytes", "na.ger", "letter.series", "relationship.sender.receiver",
                            "social.strata.sender", "social.strata.receiver", "sender.latlong.string",
                            "receiver.latlong.string", "tzone", "sender.country", "receiver.country",
                            "data.ambiguous", "sender", "receiver", "location.sender", "location.receiver"
)

## ================ Add decades column to the data =======================
## ==============================================================================

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

## ================ Handle letters where send/receive is the same =======================
## ==============================================================================

offending_duplicate_send_receive_ids <- letters_df %>%
  filter(sender.latitude == receiver.latitude) %>%
  select(id.letter) %>%
  .[[1]]

letters_df <- letters_df %>%
  filter(!id.letter %in% offending_duplicate_send_receive_ids)


## ================ Process shapefiles from statesRcontiguous =======================
## ==============================================================================

states_shapefiles <- shp_all_us_states %>%
  filter(contiguous.united.states)
counties_shapefiles <- shp_all_us_counties %>%
  filter(contiguous.united.states)
congressional_districts_shapefiles <- shp_all_us_congressional_districts %>%
  filter(contiguous.united.states)

## ================ Define selected families =======================
## ==============================================================================

selected_family_letter_series <- c("Schulz-Bruns Geisberg" = "S001", "Gerstein-Gerstein" = "G004", "Benzler-Degenhard" = "B117")

selected_families <- letters_df %>%
  filter(grepl(paste0(selected_family_letter_series, collapse = "|"), id.letter)) %>%
  select(id.letter, letter.series) %>%
  mutate(id.letter = gsub("_.*", "", id.letter)) %>%
  unique() %>%
  rename(series.id = id.letter,
         series.name = letter.series)

letters_df <- letters_df %>%
  mutate(selected.family = ifelse(grepl(paste0(selected_family_letter_series, collapse = "|"), id.letter),
                                  TRUE,FALSE))

selected_families_letters <- letters_df %>%
  filter(selected.family)


unique_selected_letter_locations <- selected_families_letters %>%
  select(sender.longitude, sender.latitude) %>%
  unique()



