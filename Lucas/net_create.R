library(tidyverse)
library(psych)
library(igraph)
library(ggpubr)

setwd("D://Documentos/Uni/Camilo/CORE_nets/Lucas")
##Función para calcular las abundancias relativas 
#(basicamente calcula abundancia total por muestra/sitio y divide la abundancia de cada asv entre esa)
Relative_Abundances<-function(data){
  totalAb<-rowSums(data)
  RelAb<-data.frame()
  for (x in 1:length(data[,1])) {
    RelAb<-rbind(RelAb,data[x,]/totalAb[x])
    RelAb[is.na(RelAb)]<-0
  }
  FinalRelAb<-data.frame("colMeans.(t)."=colMeans(RelAb),"From"=colnames(data))
} 
#####CORRELATION AND STRUCTURING DATA, RENAMING
node_t<-list()
for (y in 1:6) {
  #Llamamos a los datos, con str_glue logramos que llame a las 6 distintas
  t<-as.data.frame(read.table(str_glue("t{y}.csv"),sep=";",dec=".",h=T, row.names=1))
  #Llamamos la función de abundancias relativas
  abundance<-Relative_Abundances(data=t)
  #Análisis de correlación de Spearman ajustando el valor de p con false discovery rate
  tcor<-corr.test(t,method="spearman",adjust="fdr",alpha=.05, ci=F)
  #Guardamos el resultado con valores de r (una tabla)
  write.table(tcor$r,str_glue("net_creation/t{y}/tr{y}.csv"),sep="\t",dec=".")
  #Guardamos el resultado con valores de p (una tabla)
  write.table(tcor$p,str_glue("net_creation/t{y}/tp{y}.csv"),sep="\t",dec=".")
  #Preparamos unos objetos para el loop
  names<-NULL
  datar<-NULL
  datap<-NULL
  for(i in 1:dim(tcor$r)[1]){
    #Vector con los nombres correspondientes a cada correlacion ( ASV0001-ASV0002), con el loop recorre la matriz de correlacion registrando 
    #que cada ASV interactue con el resto  
    names<-c(names,paste(rownames(tcor$p)[i],colnames(tcor$p), sep=" - "))
    #Guardamos todos los valores de r de la matriz de correlacion siguiendo el mismo orden que para names
    datar<-c(datar, as.matrix(tcor$r)[i,])
    #Guardamos todos los valores de p de la matriz de correlacion siguiendo le mismo orden
    datap<-c(datap,as.matrix(tcor$p)[i,])
  }
  #Unimos todo en una tabla (como estaban en el mismo orden no hay problema) y filtramos por valores de p y r
  unionfr<-data.frame(ASV=names,rValue=datar,pValue=datap) %>% dplyr::filter(rValue>=0.6 | rValue<=-0.6, pValue!=0 & pValue<=0.05)
  tcytos<-data.frame(From=stringr::str_sub(unionfr[,1], start = 1, end = 7), 
                     to=stringr::str_sub(unionfr[,1], start = 11, end = 17), rValue=unionfr[,2],pValue=unionfr[,3]) %>% 
    dplyr::filter(!From==to)
  #Creamos un objeto en el que vamos a ir guardando las característica de cada nodo, partiendo por su abundancia relativa. 
  #Solo nos quedaremos con aquellos nodos que hayan presentado al menos una interacción importante (que haya pasado el filtro)
  node_t[[y]]<-abundance %>% dplyr::filter(From %in% c(tcytos$From,tcytos$to)) %>% rename("Node"="From")
  
  #write.table(node_t[[y]],str_glue("net_creation/t{y}/node_mat_t{y}.csv",sep="\t",dec=".", row.names = F))
 # write.table(tcytos,str_glue("net_creation/t{y}/adj_mat_t{y}.csv"),sep="\t",dec=".", row.names = F) 
}

#view(tcytos)
#view(node_t[[1]])

#####Node type (SHARED, UNIQUE OR OTHER ASV)
path_u<-cbind(rep("unique_", 6), c("chanaral","flamenco", "huasco", "las_cruces", "punta_choros", "quintero"), rep("_ppe_asv_abundance.csv", 6))
shared<-read.table("shared_ppe_asv_abundance.csv",sep=";",h=F)

node_t_class<-list()
ulist<-list()
for (i in 1:dim(path_u)[1]) {
ulist[[i]]<- read.table(paste0(path_u[i,1],path_u[i,2],path_u[i,3]), sep=";", h=F)  
node_t_class[[i]]<-node_t[[i]] %>%  
  mutate("Node_category"=case_when(Node %in% t(ulist[[i]])[,1]~"Unique", Node %in% t(shared[1,-1])~"Shared", TRUE~"Other"))

#write.table(node_t_class[[i]],str_glue("net_creation/t{i}/node_mat_t{i}.csv"),sep="\t",dec=".") 
}

par(mfrow=c(2,3))
ASVs<-data.frame("Other"=NA, "Shared"=NA,"Unique"=NA)
for (i in 1:6) {
  plot(table(node_t_class[[i]]$Node_category), main=path_u[i,2], ylab="ASVs")
  ASVs[i,]<-table(node_t_class[[i]]$Node_category)
}
ASVs2<-ASVs %>% mutate(Site=path_u[,2])

#write.table(ASVs2,"net_creation/other_results/ASVs_class.csv", row.names = F)

#####IGRAPH OBJECT 
gc()
edgelist<-list()
nodelist<-list()
graphlist<-list()
for (x in 1:6) {
  nodelist[[x]]<-read.table(str_glue("net_creation/t{x}/node_mat_t{x}.csv"), sep="\t",dec=".",h=T) %>% relocate(Node)
  data<-read.table(str_glue("net_creation/t{x}/adj_mat_t{x}.csv"), sep="\t",dec=".",h=T)
  #nodelist[[x]]<-node_t_class[[x]]
  edgelist[[x]]<-data %>% select(From,to)
  #nodelist[[x]]<-data %>% pivot_longer(c(From,to))  %>% distinct(value, .keep_all = T) %>% select(value,colMeans..t..,From_type,To_type)
  graphlist[[x]]<-igraph::simplify(graph_from_data_frame(edgelist[[x]], directed = F, vertices = as.data.frame(nodelist[[x]])))
}

#####FAST ANALYSIS
##Idea, evaluar assortativity para Shared vs other.
#assortativity<-NULL
cc<-NULL
avpath<-NULL
ctance<-NULL
mdeg<-NULL
modul<-NULL
clusters<-NULL
diam<-NULL
#results<-matrix(,12,1000)
size<-NULL
order<-NULL
for (x in 1:6) {
  V(graphlist[[x]])$degree<-degree(graphlist[[x]], mode = c("All"))
  V(graphlist[[x]])$eigen<-evcent(graphlist[[x]])$vector
  V(graphlist[[x]])$betweenness<-betweenness(graphlist[[x]], directed = F)
  values<-as.factor(V(graphlist[[x]])$Tipo)
  #assortativity[x]<-assortativity_nominal(graphlist[[x]], types=values)
  size[x]<-gsize(graphlist[[x]])
  order[x]<-gorder(graphlist[[x]])
  cc[x]<-transitivity(graphlist[[x]],type="average")
  avpath[x]<-mean_distance(graphlist[[x]],directed=T,unconnected=T)
  ctance[x]<-edge_density(graphlist[[x]],loops = F)
  mdeg[x]<-mean(degree(graphlist[[x]]))
  modul[x]<-modularity(fastgreedy.community(graphlist[[x]],merges = F,modularity = T))
  clusters[x]<-length(fastgreedy.community(graphlist[[x]],merges = F,modularity = T))
  diam[x]<-diameter(graphlist[[x]], directed = F, unconnected = T)
  
  #for (y in 1:1000) {
  #  results[x,y]<-assortativity_nominal(graphlist[[x]],sample(values))
  #}
}
path<-c("Chañaral","Flamenco", "Huasco", "Las Cruces", "Punta Choros", "Quintero")

nets_stats<-data.frame("Connectance"=ctance, "Average_Degree"=mdeg, "Diameter"=diam, "Average_path_length"=avpath, 
                        "Cluster_coefficient"=cc, "Modularity"=modul, "Number_of_clusters"=clusters, "Nodes"=order, 
                        "Edges"=size, "Net"=path) %>% relocate(Net)

barplot(nets_stats$Connectance~nets_stats$Net)
barplot(nets_stats$Nodes~nets_stats$Net)
barplot(nets_stats$Average_Degree~nets_stats$Net)
barplot(nets_stats$Average_path_length~nets_stats$Net)
barplot(nets_stats$Modularity~nets_stats$Net)
######NET PLOTS
sites<-c("Chañaral", "Flamenco", "Huasco", "Las_Cruces", "Punta_Choros", "Quintero")
pal<-c("Red","Green","Blue")
par(mfrow=c(2,3))
for (x in 1:6) {
  set.seed(1000)
  plot(graphlist[[x]], edge.color = 'grey', vertex.label=NA,
       #vertex.color=case_when(vertex_attr(graphlist[[x]], "Tipo")== "Eukarya"~pal[2],
        #                      vertex_attr(graphlist[[x]], "Tipo")== "Bacteria"~pal[1],
         #                     vertex_attr(graphlist[[x]], "Tipo")== "Archaea"~pal[3]),
       vertex.color=case_when(vertex_attr(graphlist[[x]], "Node_category")== "Other"~pal[2],
                              vertex_attr(graphlist[[x]], "Node_category")== "Shared"~pal[1],
                              vertex_attr(graphlist[[x]], "Node_category")== "Unique"~pal[3]),
       vertex.size = log(V(graphlist[[x]])$degree+2),edge.width=0.1/10000, layout = layout.fruchterman.reingold, 
       main=sites[x],
       sub=str_glue("N:{gorder(graphlist[[x]])}  E:{gsize(graphlist[[x]])}"))
}
par(new=F)


#### Degree plots
path<-c("Chañaral","Flamenco", "Huasco", "Las Cruces", "Punta Choros", "Quintero")

nlist<-list(); deg<-list(); top_degs<-list(); plots<-list(); nod<-list();deg_pre<-list()

for (i in 1:6) {
  nlist[[i]]<-data.frame("Node"=vertex_attr(graphlist[[i]], "name"), 
                       "Category"=vertex_attr(graphlist[[i]], "Node_category"), 
                       "Degree"=vertex_attr(graphlist[[i]], "degree", ), "Net"=path[i], "Size"=gorder(graphlist[[i]]))
  
  deg_pre[[i]]<-nlist[[i]] %>%  filter(Degree<=quantile(Degree,probs = 0.2))
  deg[[i]]<-as.data.frame(table(deg_pre[[i]]$Category)) %>% mutate("Net"=path[i], "Nodos_upperquantile"=dim(deg_pre[[i]])[1], 
                                                                   Proportion=Freq/Nodos_upperquantile,Total=Proportion)
  
  
  #top_degs[[i]]<-as.data.frame(table(deg[[i]]$Category)) 
  #plots[[i]]<-ggplot(top_degs[[i]], aes(y=Freq,x=Var1,fill=Var1)) + geom_col() + theme_bw()
  nod[[i]]<- as.data.frame(table(nlist[[i]]$Category)) %>% mutate("Net"=path[i],"Size"=gorder(graphlist[[i]]), Proportion=Freq/Size,Total=Proportion)
}
deg[[5]]<-NULL
degs<-deg %>% bind_rows()
ggplot(degs,aes(y=Total,x=Net,fill=factor(Var1,levels = c("Other","Unique","Shared")))) + geom_col() +
  scale_fill_manual(name="ASV Category",values = c("green","blue","red"))
nod[[5]]<-NULL
nods<-nod %>% bind_rows()
ggplot(nods,aes(y=Total,x=Net,fill=factor(Var1,levels = c("Other","Unique","Shared")))) + geom_col()+ 
  scale_fill_manual(name="ASV Category",values = c("green","blue","red"))

ggplot(nods,aes(y=Freq,x=Net,fill=Freq)) + geom_col() 

#ggarrange(plots[[1]],plots[[2]],plots[[3]],plots[[4]],plots[[5]],plots[[6]])

####Clustering
clusts<-NULL
mods<-NULL
for (y in 1:6) {
  fc<-fastgreedy.community(graphlist[[y]], weights = NULL)
  V(graphlist[[y]])$membership<-fc$membership
  clusts[y]<-length(fc)
  mods[y]<-modularity(graphlist[[y]],fc$membership)
}
plot(clusters)
plot(mods)

######Plots con clusters, 
##Ideas para evaluar como se reparten shared entre clusters, 1. Ver que porcentaje de las ASVs de un cluster pertenecen a cada categoría.
##2. Ver que cluster contiene la mayoría de las shared y evaluar cual es ese porcentaje.
sites<-c("Chañaral", "Flamenco", "Huasco", "Las_Cruces", "Punta_Choros", "Quintero")
par(mfrow=c(2,3))
for (x in 1:6) {
  set.seed(1000)
  plot(graphlist[[x]], edge.color = 'grey', vertex.label=NA,
       #vertex.color=case_when(vertex_attr(graphlist[[x]], "Tipo")== "Eukarya"~pal[2],
       #                      vertex_attr(graphlist[[x]], "Tipo")== "Bacteria"~pal[1],
       #                     vertex_attr(graphlist[[x]], "Tipo")== "Archaea"~pal[3]),
       vertex.color=vertex_attr(graphlist[[x]],"membership"),
       vertex.size = log(V(graphlist[[x]])$degree+2),edge.width=0.1/10000, layout = layout.fruchterman.reingold, 
       main=sites[x],
       sub=str_glue("N:{gorder(graphlist[[x]])}  E:{gsize(graphlist[[x]])}"))
}
par(new=F)


###########No recuerdo que es esto
t<-list()
for (y in 1:6) {
  t[[y]]<-as.data.frame(t(read.table(str_glue("t{y}.csv"),sep=";",dec=".",h=F, row.names=1))) %>% dplyr::select(V1)
}
test<-t %>% unlist() %>% inner_join(by="V1")

#####Un metodo para separar los nombres, por desgracia me tarda muchísimo
for (i in 1:length(names)) {
  a[i,]<-data.frame("From"=strsplit(names, " - ")[[i]][1], "to"=strsplit(names, " - ")[[i]][2])
}

#####

huas<-read.table("unique_huasco_ppe_asv_abundance.csv", sep=";")
pc<-read.table("unique_punta_choros_ppe_asv_abundance.csv", sep=";")
qint<-read.table("unique_quintero_ppe_asv_abundance.csv", sep=";")
