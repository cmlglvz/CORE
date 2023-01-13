library(tidyverse)
library(sf)
library(mapview)

#2012 Starbucks locations
starbucks <- read_csv("https://raw.githubusercontent.com/libjohn/mapping-with-R/master/data/All_Starbucks_Locations_in_the_US_-_Map.csv")

#Subset North Carolina
starbucksNC <- starbucks  %>% filter(State == "NC")
starbucksNC %>% glimpse()

#Making map
mapview(starbucksNC, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

#Transform data to spatial object
sbux_sf <- st_as_sf(starbucksNC, coords = c("Longitude", "Latitude"),  crs = 4326)

#mapview(sbux_sf)
mapview(sbux_sf, map.types = "Stamen.Toner") 