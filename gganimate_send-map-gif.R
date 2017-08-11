library(gapminder)
library(ggplot2)

## =========== us state map

us <- map_data("state")

arr <- USArrests %>% 
  rownames_to_column("region") %>% 
  mutate(region=tolower(region))

us_state_map <- ggplot() + 
  geom_map(data=us, map=us,
                    aes(x=long, y=lat, map_id=region),
                    fill="#ffffff", color="#000000", size=.3)

## =========== Add locations

send_decade_map <- us_state_map +
  geom_point(
    data = selected_families_letters %>%
      select(sender.latitude, sender.longitude) %>%
      unique(),
    aes(x = sender.longitude, y = sender.latitude),
    fill = "#c4c4c4",
    colour = "black", pch = 21, size = 5
  )

## =========== Iteratively add send locs in each decade

send_decade_map <- send_decade_map +
  geom_point(
    data = selected_families_letters %>%
      select(sender.latitude, sender.longitude, decade) %>%
      na.omit(),
    aes(x = sender.longitude, y = sender.latitude,
        frame = decade),
    fill = "#ff7f00",
    colour = "black", pch = 21, size = 5
  ) + 
  ggtitle("Selected family send location in the decade: ") + 
  theme(plot.title = element_text(size = 20, face = "bold"))

map_width <- 1210 
map_height <- 662

gganimate(send_decade_map, ani.width = map_width, ani.height = map_height,
          interval = 1,
          filename = "selected_families_send_location_GIF.gif")


