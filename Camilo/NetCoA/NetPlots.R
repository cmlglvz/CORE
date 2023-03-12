#Net plots

#Chañaral
set.seed(1000)
plot(graphlist[[1]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[1]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[1]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[1]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[1]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.kamada.kawai, 
     sub = str_glue("N:{gorder(graphlist[[1]])} E:{gsize(graphlist[[1]])}"))

set.seed(1000)
plot(graphlist[[1]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[1]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[1]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[1]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[1]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.fruchterman.reingold, 
     sub = str_glue("N:{gorder(graphlist[[1]])} E:{gsize(graphlist[[1]])}"))


#Flamenco
set.seed(1000)
plot(graphlist[[2]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[2]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[2]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[2]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[2]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.kamada.kawai, 
     sub = str_glue("N:{gorder(graphlist[[2]])} E:{gsize(graphlist[[2]])}"))

set.seed(1000)
plot(graphlist[[2]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[2]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[2]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[2]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[2]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.fruchterman.reingold, 
     sub = str_glue("N:{gorder(graphlist[[2]])} E:{gsize(graphlist[[2]])}"))


#Huasco
set.seed(1000)
plot(graphlist[[3]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[3]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[3]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[3]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[3]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.kamada.kawai, 
     sub = str_glue("N:{gorder(graphlist[[3]])} E:{gsize(graphlist[[3]])}"))

set.seed(1000)
plot(graphlist[[3]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[3]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[3]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[3]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[3]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.fruchterman.reingold, 
     sub = str_glue("N:{gorder(graphlist[[3]])} E:{gsize(graphlist[[3]])}"))


#Pta. Choros
set.seed(1000)
plot(graphlist[[4]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[4]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[4]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[4]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[4]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.kamada.kawai, 
     sub = str_glue("N:{gorder(graphlist[[4]])} E:{gsize(graphlist[[4]])}"))

set.seed(1000)
plot(graphlist[[4]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[4]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[4]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[4]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[4]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.fruchterman.reingold, 
     sub = str_glue("N:{gorder(graphlist[[4]])} E:{gsize(graphlist[[4]])}"))


#Quintero
set.seed(1000)
plot(graphlist[[5]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[5]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[5]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[5]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[5]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.kamada.kawai, 
     sub = str_glue("N:{gorder(graphlist[[5]])} E:{gsize(graphlist[[5]])}"))

set.seed(1000)
plot(graphlist[[5]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[5]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[5]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[5]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[5]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.fruchterman.reingold, 
     sub = str_glue("N:{gorder(graphlist[[5]])} E:{gsize(graphlist[[5]])}"))


#Las Cruces
set.seed(1000)
plot(graphlist[[6]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[6]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[6]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[6]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[6]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.kamada.kawai, 
     sub = str_glue("N:{gorder(graphlist[[6]])} E:{gsize(graphlist[[6]])}"))

set.seed(1000)
plot(graphlist[[6]], 
     edge.color = "dark grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[6]], "Node_Category") == "Unique"~pal[1], 
                              vertex_attr(graphlist[[6]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[6]], "Node_Category") == "Shared"~pal[3]), 
     vertex.size = log(V(graphlist[[6]])$degree+2), 
     edge.width = 0.1/10000, 
     layout = layout.fruchterman.reingold, 
     sub = str_glue("N:{gorder(graphlist[[6]])} E:{gsize(graphlist[[6]])}"))