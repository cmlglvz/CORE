library(tidyverse)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(tmap)
library(leaflet)
library(grid)
library(shiny)

#Large dataset - Raster
nz_elev <- rast(system.file("raster/nz_elev.tif", package = "spDataLarge"))
elev <- rast(system.file("raster/elev.tif", package = "spData"))
grain <- rast(system.file("raster/grain.tif", package = "spData"))

# Add fill layer to nz shape
tm_shape(nz) + tm_fill() 
# Add border layer to nz shape
tm_shape(nz) + tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) + tm_fill() + tm_borders() 
#Alternative
qtm(nz)

#Store objects representing maps (class tmap)
map_nz <- tm_shape(nz) + tm_polygons()
class(map_nz)

#Adding raster layer
map_nz1 <- map_nz + tm_shape(nz_elev) + tm_raster(alpha = 0.7)
map_nz1

#Adding more layers
nz_water <- st_union(nz) |> st_buffer(22200) |> st_cast(to = "LINESTRING")
map_nz2 <- map_nz1 + tm_shape(nz_water) + tm_lines()
map_nz2

map_nz3 <- map_nz2 + tm_shape(nz_height) + tm_dots()
map_nz3

#Aesthetics
ma1 <- tm_shape(nz) + tm_fill(col = "red")
ma2 <- tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 <- tm_shape(nz) + tm_borders(col = "blue")
ma4 <- tm_shape(nz) + tm_borders(lwd = 3)
ma5 <- tm_shape(nz) + tm_borders(lty = 2)
ma6 <- tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) + tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

#Does Work and Doesn't work examples
plot(st_geometry(nz), col = nz$Land_area)  # works
tm_shape(nz) + tm_fill(col = nz$Land_area) # fails

#Use this instead
tm_shape(nz) + tm_fill(col = "Land_area")

#Titles associated with legend
legend_title <- expression("Area (km"^2*")")
map_nza <- tm_shape(nz) + tm_fill(col = "Land_area", title = legend_title) + tm_borders()
map_nza

#Color Settings
tm_shape(nz) + tm_polygons(col = "Median_income")
breaks <- c(0, 3, 4, 5) * 10000
tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")
tm_shape(nz) + tm_polygons("Population", palette = "Blues")
tm_shape(nz) + tm_polygons("Population", palette = "YlOrBr")

#Layouts
map_nz + 
  tm_compass(type = "8star", position = c("left", "top")) + 
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)
map_nz + tm_layout(title = "New Zealand")
map_nz + tm_layout(scale = 5)
map_nz + tm_layout(bg.color = "lightblue")
map_nz + tm_layout(frame = FALSE)
map_nza + tm_style("bw")
map_nza + tm_style("classic")
map_nza + tm_style("cobalt")
map_nza + tm_style("col_blind")

#Faceted maps
urb_1970_2030 = urban_agglomerations |> filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)

#Inset maps
nz_region <- st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000), 
                     crs = st_crs(nz_height)) |> st_as_sfc()

nz_height_map <- tm_shape(nz_elev, bbox = nz_region) + 
  tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE) + 
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
  tm_scale_bar(position = c("left", "bottom"))

nz_map <- tm_shape(nz) + 
  tm_polygons() +
  tm_shape(nz_height) + 
  tm_symbols(shape = 2, col = "red", size = 0.1) + 
  tm_shape(nz_region) + 
  tm_borders(lwd = 3) 

nz_height_map
print(nz_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))

#Inset for non-contiguous areas 
us_states_map <- tm_shape(us_states, 
                          projection = "EPSG:2163") + 
  tm_polygons() + 
  tm_layout(frame = FALSE)

hawaii_map <- tm_shape(hawaii) + 
  tm_polygons() + 
  tm_layout(title = "Hawaii", 
            frame = FALSE, 
            bg.color = NA, 
            title.position = c("LEFT", "BOTTOM")
            )
alaska_map <- tm_shape(alaska) + 
  tm_polygons() + 
  tm_layout(title = "Alaska", 
            frame = FALSE, 
            bg.color = NA)

us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))

#Animated maps
urb_anim <- tm_shape(world) + 
  tm_polygons() + 
  tm_shape(urban_agglomerations) + 
  tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)

tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)

#Interactive maps
tmap_mode("view")
map_nz

map_nz + tm_basemap(server = "OpenTopoMap")

world_coffee <- left_join(world, coffee_data, by = "name_long")
facets <- c("coffee_production_2016", "coffee_production_2017")
tm_shape(world_coffee) + 
  tm_polygons(facets) + 
  tm_facets(nrow = 1, sync = TRUE)

#Going back to plot mode
tmap_mode("plot")
mapview::mapview(nz)

#Detailed example (run everything at once to work)
library(mapview)
oberfranken <- subset(franconia, district == "Oberfranken")
trails %>%
  st_transform(st_crs(oberfranken)) %>%
  st_intersection(oberfranken) %>%
  st_collection_extract("LINESTRING") %>%
  mapview(color = "red", 
          lwd = 3, 
          layer.name = "trails") +
  mapview(franconia, 
          zcol = "district", 
          burst = TRUE) +
  breweries

#Second detailed example (no me funcionó)
library(mapdeck)
set_token(Sys.getenv("MAPBOX"))
crash_data = read.csv("https://git.io/geocompr-mapdeck")
crash_data = na.omit(crash_data)
ms = mapdeck_style("dark")
mapdeck(style = ms, pitch = 45, location = c(0, 52), zoom = 4) |>
  add_grid(data = crash_data, lat = "lat", lon = "lng", cell_size = 1000,
           elevation_scale = 50, layer_id = "grid_layer",
           colour_range = hcl.colors(6, "plasma"))

#Third
pal <- colorNumeric("RdYlBu", domain = cycle_hire$nbikes)
leaflet(data = cycle_hire) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(col = ~pal(nbikes), 
             opacity = 0.9) %>% 
  addPolygons(data = lnd, 
              fill = FALSE) %>% 
  addLegend(pal = pal, 
            values = ~nbikes) %>% 
  setView(lng = -0.1, 51.5, zoom = 12) %>% 
  addMiniMap()

#Map applications
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
ui <- fluidPage(
  sliderInput(inputId = "life", 
              "Life expectancy", 
              49, 
              84, 
              value = 80),
  leafletOutput(outputId = "map")
)
server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() |> 
      # addProviderTiles("OpenStreetMap.BlackAndWhite") |>
      addPolygons(data = world[world$lifeExp < input$life, ])})
}
shinyApp(ui, server)

#Other maps
g <- st_graticule(nz, lon = c(170, 175), lat = c(-45, -40, -35))
plot(nz_water, graticule = g, axes = TRUE, col = "blue")
terra::plot(nz_elev / 1000, add = TRUE, axes = FALSE)
plot(st_geometry(nz), add = TRUE)

g1 = ggplot() + geom_sf(data = nz, aes(fill = Median_income)) +
  geom_sf(data = nz_height) +
  scale_x_continuous(breaks = c(170, 175))
g1

#Cartogram
library(cartogram)
nz_carto = cartogram_cont(nz, "Median_income", itermax = 5)
tm_shape(nz_carto) + tm_polygons("Median_income")
us_states2163 <- st_transform(us_states, "EPSG:2163")
us_states2163_ncont <- cartogram_ncont(us_states2163, "total_pop_15")
us_states2163_dorling <- cartogram_dorling(us_states2163, "total_pop_15")