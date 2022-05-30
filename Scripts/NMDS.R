#NMDS
#According to Patrick Schloss NMDS does the best job of representing the variation of the data
#metal.env tabla con concentraciones de metales para las muestras
#metal.hel tabla de ASV de PPE compartidas por los sitios transformadas a hellinger
#metal.dist tabla de distancia bray-curtis de metal.hel
#lookup = metal.env sin corrección de nombre de filas, sirve como referencia

library(tidyverse)
library(vegan)

set.seed(19910420)
nmds <- metaMDS(comm = metal.hel, distance = "bray", k = 2, trymax = 100)
str(nmds)
scrs <- vegan::scores(nmds)
nmds$points
plot(nmds)

scrs$sites %>% 
  as_tibble(rownames = "Samples") %>% 
  inner_join(., lookup, by = "Samples") %>% 
  ggplot(aes(x=NMDS1, y=NMDS2, color=Site)) + 
  geom_point()
