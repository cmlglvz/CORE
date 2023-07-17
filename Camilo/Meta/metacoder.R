#Libraries
library(tidyverse)

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
