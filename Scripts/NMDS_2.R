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

momo <- scrs$sites %>% 
  as_tibble(rownames = "Samples") %>% 
  inner_join(., lookup, by = "Samples") %>% 
  mutate(Cu.Level = if_else(Copper > 6, "Higher", "Lower")) %>% 
  mutate(Fe.Level = if_else(Iron > 10, "Higher", "Normal"))

MOMO <- ggplot(momo, aes(x = NMDS1, y = NMDS2, color = Site, size = Fe.Level)) + 
  geom_point(aes(shape = Cu.Level)) + 
  coord_fixed() + 
  scale_color_viridis()
  scale_color_manual(name = NULL, 
                     breaks = c("Cha", "Fla", "Hu", "Pc", "LC", "Qin"), 
                     values = c("#F3722C", "#FCA72D", "#4E0066", "#AA006A", "#007F5F", "#94D76B"), 
                     labels = c("Chañaral", 
                                "Flamenco", 
                                "Huasco", 
                                "Punta de Choros", 
                                "Las Cruces", 
                                "Quintero")
                     ) + 
  theme_classic()
fort <- fortify(nmds)
ggplot() +
    geom_point(data = subset(fort, Score == 'sites'),
               mapping = aes(x= NMDS1, y = NMDS2),
               colour = "black",
               alpha = 0.5) +
    geom_segment(data = subset(fort, Score == 'species'),
                 mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.015, "npc"),
                               type = "closed"),
                 colour = "darkgray", 
                 size = 0.8) +
    geom_text(data = subset(fort, Score == 'species'), #crudely push labels away
              mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1)) +
    geom_abline(intercept = 0, slope = 0, linetype = "dashed", size = 0.8, colour = "gray") +
    geom_vline(aes(xintercept = 0), linetype = "dashed", size = 0.8, colour = "gray") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  
p1 <- ggplot() +
    geom_point(data = subset(fort, Score == 'sites'),
               mapping = aes(x = NMDS1, y = NMDS2),
               colour = "black",
               alpha = 0.5) +
    geom_segment(data = subset(fort, Score == 'species'),
                 mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.015, "npc"),
                               type = "closed"),
                 colour = "darkgray",
                 size = 0,
                 alpha = 0) +
    geom_text(data = subset(fort, Score == 'species'),
              mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1),
              alpha = 0) +
    geom_abline(intercept = 0, slope = 0, linetype = "dashed", size = 0.8, colour = "gray") +
    geom_vline(aes(xintercept = 0), linetype = "dashed", size = 0.8, colour = "gray") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  
p2 <- ggplot() +
    geom_point(data = subset(fort, Score == 'sites'),
               mapping = aes(x = NMDS1, y = NMDS2),
               colour = "black",
               alpha = 0) +
    geom_segment(data = subset(fort, Score == 'species'),
                 mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.015, "npc"),
                               type = "closed"),
                 colour = "darkgray",
                 size = 0.8) +
    geom_text(data = subset(fort, Score == 'species'),
              mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1)) +
    
    geom_abline(intercept = 0, slope = 0, linetype = "dashed", size = 0.8, colour = "gray") +
    geom_vline(aes(xintercept = 0), linetype = "dashed", size = 0.8, colour = "gray") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
ggarrange(p1, p2, ncol = 1)
