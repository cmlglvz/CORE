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
