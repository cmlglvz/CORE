#PCoA
#metal.env tabla con concentraciones de metales para las muestras
#metal.hel tabla de ASV de PPE compartidas por los sitios transformadas a hellinger
#metal.dist tabla de distancia bray-curtis de metal.hel
#lookup = metal.env sin corrección de nombre de filas, sirve como referencia

library(tidyverse)
library(glue)

cmdscale(metal.dist) #to generate the ordination 
pcoa <- cmdscale(metal.dist, k=2, eig = TRUE, add = TRUE)
positions <- pcoa$points
positions %>% head()
colnames(positions) <- c("PCoA1", "PCoA2")
percent_explained <- 100 * pcoa$eig / sum(pcoa$eig)
percent_explained[1:2]
pretty_pe <- format(round(percent_explained[1:2], digits = 1), nsmall = 1, trim = TRUE)
labs <- c(glue("PCo 1 ({pretty_pe[1]}%)"), 
          glue("PCo 2 ({pretty_pe[2]}%)")
          )
positions %>% 
  as_tibble(rownames = "Samples") %>% 
  ggplot(aes(x=PCoA1, y=PCoA2)) + 
  geom_point() + 
  labs(x = labs[1], y = labs[2])

#scree plot indicate the percent explained by each axis
tibble(pe = percent_explained, 
       axis = 1:length(percent_explained)) %>% 
  ggplot(aes(x=axis, y=pe)) + 
  geom_line() + 
  coord_cartesian(xlim = c(1,10))

cumsum(percent_explained)
