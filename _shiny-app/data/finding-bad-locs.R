

given_locs <- locations_df %>%
  select(latitude, longitude)

letter_sends <- letters_df %>%
  select(sender.latitude, sender.longitude) %>%
  setNames(c("latitude", "longitude"))

letter_receives <- letters_df %>%
  select(receiver.latitude, receiver.longitude) %>%
  setNames(c("latitude", "longitude"))

letters_locs <- letter_sends %>%
  bind_rows(letter_receives) %>%
  unique()



unspecified_locations <- letters_locs %>%
  anti_join(given_locs) %>%
  na.omit()



unspecified_sends <- letters_df %>%
  inner_join(unspecified_locations, by = c("sender.latitude" = "latitude",
                                           "sender.longitude" = "latitude")) %>%
  select(sender.location, sender.latitude, sender.longitude) %>%
  unique() %>%
  setNames(c("location", "latitude", "longitude"))


unspecified_receives <- letters_df %>%
  inner_join(unspecified_locations, by = c("receiver.latitude" = "latitude",
                                           "receiver.longitude" = "latitude")) %>%
  select(receiver.location, receiver.latitude, receiver.longitude) %>%
  unique() %>%
  setNames(c("location", "latitude", "longitude"))

unspecified_sends %>%
  bind_rows(unspecified_receives) %>%
  unique() %>%
  write_csv("data/unspecified_locations.csv")
