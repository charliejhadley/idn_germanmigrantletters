## =========== world map

world <- map_data("world")

arr <- USArrests %>% 
  rownames_to_column("region") %>% 
  mutate(region=tolower(region))

world_map <- ggplot() + 
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           fill="#ffffff", color="#000000", size=.3)

world_map +
  coord_map(xlim = c(-130, 31), ylim = c(21, 56),
            projection = "vandergrinten") + 
  scale_x_continuous(breaks = NULL)

## =========== world map

download.file("https://gist.githubusercontent.com/hrbrmstr/467640f8da38bc01ef35/raw/092728bc33b40b8bf7fafbfa74a1e4a96335241e/ne_50m_admin_0_countries.geojson", destfil = "data/natural-earth-geojson.geojson")

natural_earth_map <- read_sf("data/natural-earth-geojson.geojson")

world_map <- natural_earth_map %>%
  ggplot() +
  geom_sf()


world_map +
  geom_point(
    data = selected_families_letters %>%
      select(sender.latitude, sender.longitude) %>%
      unique(),
    aes(x = sender.longitude, y = sender.latitude),
    fill = "#c4c4c4",
    colour = "black", pch = 21, size = 2
  )

## =========== Extract journey coords

selected_family_journies <- letter_journey_lines(letters_df, selected.family = "only", unique.or.all = "all")

selected_family_journies <- selected_family_journies %>%
  filter(!is.na(decade))

## =========== Add blank send locations in the states

journey_decade_map <- world_map +
  geom_point(
    data = selected_families_letters %>%
      select(sender.latitude, sender.longitude) %>%
      unique(),
    aes(x = sender.longitude, y = sender.latitude),
    fill = "#c4c4c4",
    colour = "black", pch = 21, size = 3
  )


## =========== Animated journies

journey_decade_map <- journey_decade_map +
  geom_sf(data = selected_family_journies %>%
            select(decade),
          aes(frame = decade)) + 
  geom_point(
    data = selected_families_letters %>%
      select(sender.latitude, sender.longitude, decade) %>%
      na.omit(),
    aes(x = sender.longitude, y = sender.latitude,
        frame = decade),
    fill = "#ff7f00",
    colour = "black", pch = 21, size = 3
  ) +
  coord_sf(xlim = c(-130, 31), ylim = c(21, 70)) +
  ggtitle("Selected family letter journies in the decade: ") + 
  theme(plot.title = element_text(size = 20, face = "bold"))

map_width <- 1030 
map_height <- 611

gganimate(journey_decade_map, 
          ani.width = map_width, ani.height = map_height,
          interval = 1,
          filename = "selected_families_journies_GIF.gif")
