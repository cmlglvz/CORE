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
seqRFLP::dataframe2fas(sha.fasta, "./Camilo/Meta/Data/hcs_shared_ppe_asv.fasta")

malign <- "./Camilo/Meta/Data/hcs_shared_ppe_multialign.fasta"
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
#Time difference of 1144.36 secs
dbDisconnect(dbConn)

