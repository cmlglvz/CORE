#Desde Core Microbiota, stringent data
#Taxa correspondiente

id.stringent.abun <- stringent.abund
colnames(id.stringent.abun) <- ATs$ASV
id.shared.abun <- shared.asv
colnames(id.shared.abun) <- shared$X
write.csv2(id.shared.abun, "./Camilo/Analisis/network/stringent_shared_abundance.csv")

id.cha.asv <- cha.asv
colnames(id.cha.asv) <- chanaral$X
write.csv2(id.cha.asv, "./Camilo/Analisis/network/unique_001.csv")

id.fla.asv <- fla.asv
colnames(id.fla.asv) <- flamenco$X
write.csv2(id.fla.asv, "./Camilo/Analisis/network/unique_002.csv")

id.hua.asv <- hu.asv
colnames(id.hua.asv) <- huasco$X 
write.csv2(id.hua.asv, "./Camilo/Analisis/network/unique_003.csv")

id.pch.asv <- pch.asv
colnames(id.pch.asv) <- choros$X
write.csv2(id.pch.asv, "./Camilo/Analisis/network/unique_004.csv")

id.qin.asv <- qin.asv
colnames(id.qin.asv) <- quintero$X
write.csv2(id.qin.asv, "./Camilo/Analisis/network/unique_005.csv")

id.lcs.asv <- lc.asv
colnames(id.lcs.asv) <- cruces$X
write.csv2(id.lcs.asv, "./Camilo/Analisis/network/unique_006.csv")

rest <- dplyr::select(id.stringent.abun, -all_of(shared$X)) %>% 
  dplyr::select(-all_of(chanaral$X)) %>% 
  dplyr::select(-all_of(flamenco$X)) %>% 
  dplyr::select(-all_of(huasco$X)) %>% 
  dplyr::select(-all_of(choros$X)) %>% 
  dplyr::select(-all_of(quintero$X)) %>% 
  dplyr::select(-all_of(cruces$X))
write.csv2(rest, "./Camilo/Analisis/network/others.csv")

chan <- aus.pres %>% dplyr::filter(Chanaral == 1)
id.chabund <- dplyr::select(id.stringent.abun, all_of(chan$X))
write.csv2(id.chabund, "./Camilo/Analisis/network/t1.csv")

flan <- aus.pres %>% dplyr::filter(Flamenco == 1)
id.flabund <- dplyr::select(id.stringent.abun, all_of(flan$X))
write.csv2(id.flabund, "./Camilo/Analisis/network/t2.csv")

huan <- aus.pres %>% dplyr::filter(Huasco == 1)
id.huabund <- dplyr::select(id.stringent.abun, all_of(huan$X))
write.csv2(id.huabund, "./Camilo/Analisis/network/t3.csv")

pchn <- aus.pres %>% dplyr::filter(Pta.Choros == 1)
id.chorond <- dplyr::select(id.stringent.abun, all_of(pchn$X))
write.csv2(id.chorond, "./Camilo/Analisis/network/t4.csv")

quin <- aus.pres %>% dplyr::filter(Quintero == 1)
id.quind <- dplyr::select(id.stringent.abun, all_of(quin$X))
write.csv2(id.quind, "./Camilo/Analisis/network/t5.csv")

crucen <- aus.pres %>% dplyr::filter(LasCruces == 1)
id.crucend <- dplyr::select(id.stringent.abun, all_of(crucen$X))
write.csv2(id.crucend, "./Camilo/Analisis/network/t6.csv")

