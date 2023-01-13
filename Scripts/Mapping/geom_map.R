library(tidyverse)
# create data for world coordinates using
# map_data() function
world_coordinates <- map_data("world")

# create world map using ggplot() function
ggplot() +
  # geom_map() function takes world coordinates
  # as input to plot world map
  geom_map(data = world_coordinates, 
           map = world_coordinates, 
           aes(long, lat, map_id = region)
           )


###Colors
ggplot() +
  # geom_map() function takes world coordinates as input
  # to plot world map color parameter determines the
  # color of borders in map fill parameter determines
  # the color of fill in map size determines the thickness
  # of border in map
  geom_map(data = world_coordinates, 
           map = world_coordinates, 
           aes(long, lat, map_id = region), 
           color = "white", 
           fill = "darkgreen", 
           size = 0.2
           )

###Data over map
#Volvano data
volcano_eruption <- readr::read_csv("Downloads/volcano.csv")
ggplot() + 
  geom_map(data = world_coordinates, map = world_coordinates,
  aes(long, 
      lat, 
      map_id = region), 
  color = "green", 
  fill= "lightyellow"
) +
  # geom_point function is used to plot scatter plot on top of world map
  geom_point(data = volcano_eruption, 
             aes(longitude, 
                 latitude, 
                 color = primary_volcano_type, 
                 size = population_within_10_km), 
             alpha = 1
             ) +
  #legend.position as none removes the legend
  theme(legend.position = "none")


###External option
WorldData <- filter(world_coordinates, region != "Antarctica") %>% fortify()
df <- data.frame(region = c('Hungary', 
                            'Lithuania', 
                            'Argentina'), 
                 value = c(4, 
                           10, 
                           11), 
                 stringsAsFactors = FALSE)

p <- ggplot() + 
  geom_map(data = WorldData, 
           map = WorldData, 
           aes(x = long, 
               y = lat, 
               group = group, 
               map_id = region), 
           fill = "white", 
           colour = "#7f7f7f", 
           size = 0.5) +  
  geom_map(data = df, 
           map = WorldData, 
           aes(fill = value, 
               map_id = region), 
           colour = "#7f7f7f", 
           size = 0.5) + 
  coord_map("rectangular", 
            lat0 = 0, 
            xlim = c(-180, 180), 
            ylim = c(-60, 90)
            ) + 
  scale_fill_continuous(low = "thistle2", 
                        high ="darkred", 
                        guide = "colorbar") + 
  scale_y_continuous(breaks = c()) + 
  scale_x_continuous(breaks=c()) + 
  labs(fill = "legend", 
       title = "Title", 
       x = "", 
       y = "") + 
  theme_bw()
p

### Otro ejemplo
if (require(maps)) {
  crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
  # Equivalent to crimes %>% tidyr::pivot_longer(Murder:Rape)
  vars <- lapply(names(crimes)[-1], function(j) {
    data.frame(state = crimes$state, variable = j, value = crimes[[j]])
  })
  crimes_long <- do.call("rbind", vars)
  states_map <- map_data("state")
  # without geospatial coordinate system, the resulting plot
  # looks weird
  ggplot(crimes, aes(map_id = state)) +
    geom_map(aes(fill = Murder), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat)
  # in combination with `coord_sf()` we get an appropriate result
  ggplot(crimes, aes(map_id = state)) +
    geom_map(aes(fill = Murder), map = states_map) +
    # crs = 5070 is a Conus Albers projection for North America,
    #   see: https://epsg.io/5070
    # default_crs = 4326 tells coord_sf() that the input map data
    #   are in longitude-latitude format
    coord_sf(
      crs = 5070, default_crs = 4326,
      xlim = c(-125, -70), ylim = c(25, 52)
    )
  ggplot(crimes_long, aes(map_id = state)) +
    geom_map(aes(fill = value), map = states_map) +
    coord_sf(
      crs = 5070, default_crs = 4326,
      xlim = c(-125, -70), ylim = c(25, 52)
    ) +
    facet_wrap(~variable)
}