#Libraries
library(tidyverse)
library(DECIPHER)
library(dendextend)
library(vegan)
library(gtools)
library(heatmaply)
library(htmlwidgets)

#Data
data <- read.csv("./Camilo/Meta/Data/HCS_PPE_Taxonomic_And_Abundance_V2.csv", 
                 header = TRUE, 
                 sep = ";",  
                 skip = 0, 
                 row.names = 1)

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

