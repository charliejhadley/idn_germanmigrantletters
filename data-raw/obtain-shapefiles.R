library("tidyverse")
library("sf")

download.file(url = "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cd115_500k.zip",
              destfile = "data-raw/shapefiles/congressional-districts.zip")
unzip(zipfile = "data-raw/shapefiles/congressional-districts.zip", exdir = "data-raw/shapefiles/congressional-districts")
unlink("data-raw/shapefiles/congressional-districts.zip")

library("sp")
library("rgdal")

foo <- readOGR("data-raw/shapefiles/congressional-districts/")

foo@data %>%
  head()

congressional_districts_shapefiles <- read_sf("data-raw/shapefiles/congressional-districts/")
congressional_districts_shapefiles <- as(congressional_districts_shapefiles, "Spatial")


congressional_districts_shapefiles@data %>%
  head()

contiguous_counties_spdf@data %>%
  head()

save(congressional_districts_shapefiles, file = "data/congressional_districts_shapefiles.rdata")




download.file(url = "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_cd115_500k.zip",
              destfile = "data-raw/shapefiles/congressional-districts.zip")
unzip(zipfile = "data-raw/shapefiles/congressional-districts.zip")



load("data/contiguous_counties_spdf.rdata")
