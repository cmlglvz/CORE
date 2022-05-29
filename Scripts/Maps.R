library(tidyverse)
library(leaflet)
library(htmlwidgets)

stationsmap <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = -23.86725804882807, lat = 12.726777153022745, zoom = 2.5) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addCircleMarkers(lng = -70.667222, lat = -26.224278, color = "#672146", radius = 3.5, opacity = 1, label = "Chañaral") %>% 
  addCircleMarkers(lng = -70.7, lat = -26.548611, color = "#672146", radius = 3.5, opacity = 1, label = "Flamenco") %>% 
  addCircleMarkers(lng = -71.210278, lat = -28.336944, color = "#672146", radius = 3.5, opacity = 1, label = "Huasco") %>% 
  addCircleMarkers(lng = -71.4975, lat = -29.223333, color = "#672146", radius = 3.5, opacity = 1, label = "Pta. Choros") %>% 
  addCircleMarkers(lng = -71.519083, lat = -32.7555, color = "#672146", radius = 3.5, opacity = 1, label = "Quintero") %>% 
  addCircleMarkers(lng = -71.624688, lat = -33.500600, color = "#672146", radius = 3.5, opacity = 1, label = "Las Cruces") %>% 
  addCircleMarkers(lng = -6.5669, lat = 36.5533, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_004") %>% 
  addCircleMarkers(lng = 1.9378, lat = 37.051, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_007") %>% 
  addCircleMarkers(lng = 2.7996, lat = 41.6686, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_011") %>% 
  addCircleMarkers(lng = 17.4155, lat = 39.8386, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_022") %>% 
  addCircleMarkers(lng = 17.9348, lat = 42.4552, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_024") %>% 
  addCircleMarkers(lng = 19.3905, lat = 39.3888, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_025") %>% 
  addCircleMarkers(lng = 20.1705, lat = 38.4761, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_026") %>% 
  addCircleMarkers(lng = 32.898, lat = 33.9179, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_030") %>% 
  addCircleMarkers(lng = 34.835, lat = 27.16, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_031") %>% 
  addCircleMarkers(lng = 37.2183, lat = 23.36, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_032") %>% 
  addCircleMarkers(lng = 39.8567, lat = 18.4417, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_034") %>% 
  addCircleMarkers(lng = 63.5047, lat = 20.8183, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_036") %>% 
  addCircleMarkers(lng = 69.9776, lat = 14.6059, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_041") %>% 
  addCircleMarkers(lng = 73.8955, lat = 6.0001, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_042") %>% 
  addCircleMarkers(lng = 53.9801, lat = -16.957, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_052") %>% 
  addCircleMarkers(lng = 37.9889, lat = -29.5019, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_064") %>% 
  addCircleMarkers(lng = 26.2868, lat = -35.1728, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_065") %>% 
  addCircleMarkers(lng = 17.9189, lat = -34.9449, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_066") %>% 
  addCircleMarkers(lng = 17.7103, lat = -32.2401, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_067") %>% 
  addCircleMarkers(lng = -35.1803, lat = -20.9354, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_076") %>% 
  addCircleMarkers(lng = -43.2899, lat = -30.1367, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_078") %>% 
  addCircleMarkers(lng = -58.2902, lat = -47.1863, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_082") %>% 
  addCircleMarkers(lng = -85.1545, lat = -5.2529, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_102") %>% 
  addCircleMarkers(lng = -84.5766, lat = 1.9928, color = "#DAAB00", radius = 3.5, opacity = 1, label = "TARA_109") %>% 
  addCircleMarkers(lng = -3.9375, lat = 48.7778, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_002") %>% 
  addCircleMarkers(lng = 7.9, lat = 54.18194, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_003") %>% 
  addCircleMarkers(lng = 3.14, lat = 42.49, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_014") %>% 
  addCircleMarkers(lng = 5.1746, lat = 43.22639, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_022") %>% 
  addCircleMarkers(lng = 23.2538, lat = 59.8822, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_030") %>% 
  addCircleMarkers(lng = -80.09315, lat = 26.10293, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_037") %>% 
  addCircleMarkers(lng = -79.89954, lat = 32.7524, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_039") %>% 
  addCircleMarkers(lng = 13.33189, lat = 45.325568, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_049") %>% 
  addCircleMarkers(lng = -69.6409, lat = 43.8444, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_054") %>% 
  addCircleMarkers(lng = -69.5781, lat = 43.8604, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_055") %>% 
  addCircleMarkers(lng = -79.16763,	lat = 33.32306, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_060") %>% 
  addCircleMarkers(lng = 10, lat = 54.8333, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_072") %>% 
  addCircleMarkers(lng = 12.935,	lat = 43.9475, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_076") %>% 
  addCircleMarkers(lng = 13.0731, lat = 43.8514, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_077") %>% 
  addCircleMarkers(lng = 13.71003, lat = 45.70092, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_099") %>% 
  addCircleMarkers(lng = 32.954, lat = 32.822, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_123") %>% 
  addCircleMarkers(lng = 34.843, lat = 32.0694, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_132") %>% 
  addCircleMarkers(lng = 5.11504, lat = 60.16121, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_141") %>% 
  addCircleMarkers(lng = -81.01667, lat = 31.98282, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_143") %>% 
  addCircleMarkers(lng = -54.266, lat = -34.616, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_149") %>% 
  addCircleMarkers(lng = -54.2752, lat = -34.666, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_150") %>% 
  addCircleMarkers(lng = -63.6403, lat = 44.6936, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_152") %>% 
  addCircleMarkers(lng = -4.552, lat = 48.359, color = "#00567E", radius = 3.5, opacity = 1, label = "OSD_159") %>% 
  addMeasure(position = "bottomright", primaryLengthUnit = "kilometers") %>% 
  addScaleBar(position = "bottomleft")
saveWidget(stationsmap, file = "Scripts/mapa_2.html")
