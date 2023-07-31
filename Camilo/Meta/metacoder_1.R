#Libraries
library(tidyverse)
library(data.table)
library(Biostrings)
library(DECIPHER)
library(vegan)
library(UpSetR)
library(leaflet)
library(dendextend)
library(treemap)
library(heatmaply)
library(htmlwidgets)
library(hrbrthemes)

#Data
data <- read.csv("./Camilo/Meta/Data/HCS_PPE_Taxonomic_And_Abundance_V2.csv", 
                 header = TRUE, 
                 sep = ";",  
                 skip = 0, 
                 row.names = 1)
pr2v5 <- read.csv("./Camilo/Meta/Data/HCS_PPE_taxa_assign_pr2v5.csv", 
                  header = TRUE, 
                  sep = ";", 
                  skip = 0, 
                  row.names = 1)
print(all(rownames(data)%in%rownames(pr2v5)))
abund <- as.data.frame(t(data[,c(3:65)]))
taxa <- data[,c(1:2,73:80)]

#FASTA
nms <- paste(data$ASV, data$Species, sep = "_")
fasta <- data.frame(names = nms, sequences = data$Seq)
seqRFLP::dataframe2fas(fasta, "./Camilo/Meta/Data/hcs_ppe_asv.fasta")

#ALIGNMENT & DENDROGRAM (autores de DECIPHER actualizaron las funciones y cambio todo...)
multialign <- "./Camilo/Meta/Data/hcs_ppe_multialign.afa"
dbConn <-dbConnect(SQLite(), ":memory:")
Seqs2DB(multialign, type = "FASTA", dbFile = dbConn, "")
x <- dbGetQuery(dbConn, "select description from Seqs")$description
Add2DB(myData = data.frame(identifier = x, stringsAsFactors = FALSE), dbConn)
consensus <- IdConsensus(dbConn, threshold = 0.3, minInformation = 0.1)
distance.matrix <- DistanceMatrix(consensus, correction = "Jukes-Cantor", processors = NULL, verbose = TRUE)
dendro <- TreeLine(myDistMatrix = distance.matrix, 
                      method = "ML", 
                      showPlot = TRUE, 
                      type = "dendrogram", 
                      myXStringSet = consensus, 
                      processors = NULL, 
                      verbose = TRUE)
#attributes(dendrogram)
#Selected model = "K80+G4"
#Transition rates = 3.921
#Transversion rates = 1
#Alpha = 0.360
#Time difference of 88481.48 secs
dbDisconnect(dbConn)

#Dendrogram edition
clust <- as.dendrogram(as.hclust(dendrogram)) %>% dendextend::set("branches_lwd", 0.3) %>% dendextend::ladderize(right = TRUE) 
plot(clust)

#Heatmapping
simp <- abund
colnames(simp) <- nms
hell <- decostand(simp,method = "hellinger", MARGIN = 1)
#labdsv::hellinger(simp) alternative
bray <- vegdist(hell, method = "bray", diag = TRUE)
maligned <-readDNAMultipleAlignment(multialign, format = "fasta")
smatrix <- as.matrix(maligned) %>% rownames() %>% as.vector()
print(all(colnames(hell)%in%gtools::mixedsort(smatrix, decreasing = FALSE)))
heatmap <- heatmaply(normalize(hell), 
                     Colv = clust,
                     Rowv = FALSE, 
                     main = "HCS PPE ASVs with sequences clustered by maximum likelihood",
                     color = viridis(n = 256, 
                                     alpha = 1, 
                                     begin = 0, 
                                     end = 1, 
                                     option = "rocket")
)
saveWidget(heatmap, "./Camilo/Meta/Products/heatmap.html")
#Hasta acá la wea queda malísima, veamos que sucede cuando identifico o lo hago sólo con las compartidas.

#Shared sequences across all sites
prePA <- as.data.frame(t(abund))
prePA <- dplyr::mutate(prePA,
                       CHA = rowSums(prePA[c(1:12)]),
                       FLA = rowSums(prePA[c(13:24)]),
                       HUA = rowSums(prePA[c(25:36)]),
                       PCH = rowSums(prePA[c(37:41)]),
                       QUI = rowSums(prePA[c(42:51)]),
                       LCS = rowSums(prePA[c(52:63)]),
                       Total = rowSums(prePA[c(1:63)]),
                       .after = "L4A18")
PreAbs <- decostand(prePA, method = "pa")
shared <- dplyr::filter(PreAbs, CHA == 1 & FLA == 1 & HUA == 1 & PCH == 1 & QUI == 1 & LCS == 1)
sha.abund <- dplyr::select(abund, rownames(shared))
sha.taxa <- taxa[taxa$Seq%in%rownames(shared),]

#FASTA SHARED PPE
nombres <- paste(sha.taxa$ASV, sha.taxa$Species, sep = "_")
sha.fasta <- data.frame(names = nombres, sequences = sha.taxa$Seq)
seqRFLP::dataframe2fas(sha.fasta, "./Camilo/Meta/Data/hcs_shared_ppe_asv_v3.fasta")

malign <- "./Camilo/Meta/Data/hcs_v3.fasta"
dbConn <-dbConnect(SQLite(), ":memory:")
Seqs2DB(malign, type = "FASTA", dbFile = dbConn, "")
x <- dbGetQuery(dbConn, "select description from Seqs")$description
Add2DB(myData = data.frame(identifier = x, stringsAsFactors = FALSE), dbConn)
consensus <- IdConsensus(dbConn, threshold = 0.3, minInformation = 0.1)
distance.matrix <- DistanceMatrix(consensus, correction = "Jukes-Cantor", processors = NULL, verbose = TRUE)
dendro <- TreeLine(myDistMatrix = distance.matrix, 
                   method = "ML", 
                   showPlot = TRUE, 
                   type = "dendrogram", 
                   myXStringSet = consensus, 
                   processors = NULL, 
                   verbose = TRUE)
#attributes(dendro)
#Selected model = "HKY85+G4"
#Model parameters:
    #Frequency(A) = 0.180
    #Frequency(C) = 0.210
    #Frequency(G) = 0.290
    #Frequency(T) = 0.320
    #Transition rates = 3.336
    #Transversion rates = 1
    #Alpha = 0.342
#Time difference of 691.58 secs
dbDisconnect(dbConn)

#Dendrogram edition
iclust <- as.dendrogram(as.hclust(dendro)) %>% dendextend::set("branches_lwd", 0.3) %>% dendextend::ladderize(right = TRUE) 
plot(iclust)

#Heatmapping
incel <- sha.abund
colnames(incel) <- nombres
taligned <-readDNAMultipleAlignment(malign, format = "fasta")
tmatrix <- as.matrix(taligned) %>% rownames() %>% as.vector()
print(all(colnames(incel)%in%gtools::mixedsort(tmatrix, decreasing = FALSE)))
sortincel <- incel[, rownames(as.matrix(taligned))]
ihell <- decostand(sortincel,method = "hellinger")
#labdsv::hellinger(simp) alternative
brayc <- vegdist(ihell, method = "bray", diag = TRUE)

calor <- heatmaply(normalize(ihell), Colv = dendro, Rowv = FALSE, colors = viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "rocket"), main = "Shared HCS PPE ASVs clustered by maximum likelihood")

dclust <- as.dendrogram(as.hclust(dendro)) %>% dendextend::set("branches_lwd", 0.3) %>% dendextend::ladderize(right = TRUE)
plot(dclust)

calorcito <- heatmaply(normalize(ihell), 
                       Colv = dclust, 
                       Rowv = FALSE, 
                       colors = viridis(n = 256, 
                                        alpha = 1, 
                                        begin = 0, 
                                        end = 1, 
                                        direction = -1, 
                                        option = "rocket"), 
                       main = "Shared HCS PPE ASVs clustered by maximum likelihood")

saveWidget(calorcito, "./Camilo/Meta/Products/heatmap_shared_asv.html")

##metacoder
rltv <- vegan::decostand(simp, method = "total")
taxa[is.na(taxa)] <- "Unclassified"
rownames(taxa) <- taxa$ASV
colnames(rltv) <- taxa$ASV
rltv <- as.matrix(rltv)
taxa <- as.matrix(taxa)

axat <- metacoder::parse_tax_data(tax_data = as_tibble(taxa), 
                                  datasets = list(Rltv.Abundance = as_tibble(as.data.frame(t(rltv)), rownames = "ASV")), 
                                  mappings = c("ASV" = "ASV"), 
                                  class_cols = 3:10)
axat$data$type_abund <- calc_taxon_abund(axat, 
                                         "Rltv.Abundance", 
                                         cols = rownames(rltv), 
                                         groups = c(rep("Chanaral", 12), 
                                                    rep("Flamenco", 12), 
                                                    rep("Huasco", 12), 
                                                    rep("Pta_Choros", 5), 
                                                    rep("Quintero", 10), 
                                                    rep("Las_Cruces", 12))
                                         )
axat

set.seed(420)
axat %>% filter_taxa(Chanaral >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Chanaral, 
            node_color = Chanaral, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Chañaral")

set.seed(420) 
axat %>% filter_taxa(Flamenco >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Flamenco, 
            node_color = Flamenco, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Flamenco")

set.seed(420) 
axoat %>% filter_taxa(Flamenco >= 1) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Flamenco, 
            node_color = Flamenco, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Flamenco")

set.seed(420) 
axoat %>% filter_taxa(Huasco >= 1) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Huasco, 
            node_color = Huasco, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Huasco")

set.seed(420) 
axoat %>% filter_taxa(Pta.Choros >= 1) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Pta.Choros, 
            node_color = Pta.Choros, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Pta.Choros")

set.seed(420) 
axoat %>% filter_taxa(Quintero >= 1) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Quintero, 
            node_color = Quintero, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold",  
            title = "Taxa in Quintero")

set.seed(420) 
axoat %>% filter_taxa(Las.Cruces >= 1) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Las.Cruces, 
            node_color = Las.Cruces, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Las Cruces")

set.seed(2) 
axat %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Total, 
            layout = "kamada-kawai", 
            node_color = Total, 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in HCS")

identification <- add_column(simp, Total = rowSums(simp[1:63]))
write.csv2(identification, "~/Desktop/LOCURILLA/rare_taxa.csv")

plot_richness(hcs, color = "Date", x = "Samples", measures = "Chao1")
plot_richness(hcs, color = "Date", x = "Samples", measures = "ACE")
plot_richness(hcs, color = "Date", x = "Samples", measures = "Shannon")
plot_richness(hcs, color = "Date", x = "Samples", measures = "Simpson")
plot_richness(hcs, color = "Date", x = "Samples", measures = "InvSimpson")
plot_richness(hcs, color = "Date", x = "Samples", measures = "Fisher")

rltv.Otu.Table <- function(x) {
  x.Data.rltv <- NULL
  for (i in 1:dim(x)[1]) {
    x.Data.rltv <- rbind(x.Data.rltv, x[i,]/apply(x, 1, function(x) sum(x))[i])
  }
  rownames(x.Data.rltv) <- rownames(x)
  invisible(x.Data.rltv)
}

rltv <- vegan::decostand(simp, method = "total")
axoltl <- parse_tax_data(tax_data = as_tibble(taxa), 
                         datasets = list(Abundance = as_tibble(as.data.frame(t(rltv)), rownames = "ASV")), 
                         mappings = c("ASV" = "ASV"), 
                         class_cols = 3:10)
axoltl$data$type_abund <- calc_taxon_abund(axoltl, "Abundance", cols = environ$Samples, groups = environ$Site)

set.seed(420)
axoltl %>% filter_taxa(Chanaral >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Chanaral, 
            node_color = Chanaral, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Chañaral")

set.seed(420) 
axoltl %>% filter_taxa(Flamenco >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Flamenco, 
            node_color = Flamenco, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Flamenco")

set.seed(420) 
axoltl %>% filter_taxa(Huasco >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Huasco, 
            node_color = Huasco, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Huasco")

set.seed(420) 
axoltl %>% filter_taxa(Pta.Choros >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Pta.Choros, 
            node_color = Pta.Choros, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Pta.Choros")

set.seed(420) 
axoltl %>% filter_taxa(Quintero >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Quintero, 
            node_color = Quintero, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold",  
            title = "Taxa in Quintero")

set.seed(420) 
axoltl %>% filter_taxa(Las.Cruces >= 0) %>% 
  heat_tree(node_label = taxon_names, 
            node_size = Las.Cruces, 
            node_color = Las.Cruces, 
            layout = "kamada-kawai", 
            initial_layout = "fruchterman-reingold", 
            title = "Taxa in Las Cruces")