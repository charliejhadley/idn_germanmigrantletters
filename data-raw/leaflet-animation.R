locations_df

us_points <- locations_df %>%
  filter(country == "USA")

leaflet(options = leafletOptions(minZoom = 3.4)) %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addCircleMarkers(
    data = us_points,
    radius = 2
  ) %>%
  fitBounds(
    lng1 = -125.0011,
    lat1 =  24.9493,
    lng2 = -66.9326,
    lat2 =  49.5904
  ) %>%
  setMaxBounds(
    lng1 = -125.0011,
    lat1 =  24.9493,
    lng2 = -66.9326,
    lat2 =  49.5904
  )