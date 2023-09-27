catmem <- read.csv("~/Documents/GitHub/CORE/Camilo/NetCoA/category_membership.csv", 
                   header = TRUE, skip = 0, sep = ";", dec = ".", row.names = 1)
#mnl <- mnl[!mnl$ASV%in%manual,]
sha.tax <- ppe.taxa[ppe.taxa$ASV%in%shappe$X, ]
cha.tax <- ppe.taxa[ppe.taxa$ASV%in%chappe$X, ]
ucha.tax <- ppe.taxa[ppe.taxa$ASV%in%u.chappe$X,]
asentec <- cha.tax[!cha.tax$ASV%in%catmem$ASV, ] #Ahora se cuales son las ASV que faltan
qles <- print(sha.tax$ASV%in%asentec$ASV) %>% as.matrix()
uqles <- print(ucha.tax$ASV%in%asentec$ASV) %>% as.matrix()

cha.nodes <- nodelist[[1]] %>% as.matrix()

set.seed(20)
plot(graphlist[[1]], 
     edge.color = "grey", 
     vertex.label = NA, 
     vertex.color = case_when(vertex_attr(graphlist[[1]], "Node_Category") == "Other"~pal[2], 
                              vertex_attr(graphlist[[1]], "Node_Category") == "Shared"~pal[3], 
                              vertex_attr(graphlist[[1]], "Node_Category") == "Unique"~pal[1]), 
     vertex_size = log((V(graphlist[[1]])$degree+2)/100), edge.width = 0.1, layout = layout.gem, 
     main = sites[1], sub = str_glue("N:{gorder(graphlist[[1]])}  E:{gsize(graphlist[[1]])}"))

layout_nicely(graphlist[[1]], dim = 2)

cytoscapePing()
