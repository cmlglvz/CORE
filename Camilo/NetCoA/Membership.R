#Membership

##Chañaral
cha <- graphlist[[1]]
set.seed(1000)
plot(cha, 
     edge.color = "dark grey", 
     vertex.label = V(cha)$membership, 
     vertex.label.cex = 0.8, 
     vertex.color = vertex_attr(cha, "membership"), 
     vertex.size = log(V(cha)$degree+2), 
     edge.width = 0.1, 
     layout = layout.fruchterman.reingold, 
     sub = str_glue("N:{gorder(cha)} E:{gsize(cha)}"))

set.seed(1000)
plot(cha, 
     edge.color = "dark grey", 
     vertex.label = NA,  
     vertex.color = vertex_attr(cha, "membership"), 
     vertex.size = log(V(cha)$degree+2), 
     edge.width = 0.1, 
     layout = layout.kamada.kawai, 
     sub = str_glue("N:{gorder(cha)} E:{gsize(cha)}"))

##Rudimentario
memb <- V(cha) %>% as.matrix() %>% as.data.frame()
ship <- V(cha)$membership %>% as.matrix() %>% as.data.frame()
mship <- data.frame(ASV = rownames(memb), membership = ship$V1)

write.csv2(mship, "membership.csv")

siento <- vertex_attr(cha, "Node_Category") %>% as.matrix() %>% as.data.frame()
write.csv2(siento, "siento.csv")
