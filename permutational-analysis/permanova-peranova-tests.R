library(vegan)
library(EcolUtils)
library(tidyverse)
library(pairwiseAdonis)
library(lmPerm)
library(ggpubr)

set.seed(1998)

setwd("~/Documentos/Felipe/Functional_groundwater/")

d <- Sys.Date() %>% str_replace_all("-","_")

#Permanova pairwise for the entire community - functional

data <- read.csv("inputs/functional_geral_matrix_lvl4_2021_12_16.csv", check.names=FALSE) %>% as_tibble()
data <- data %>% mutate(category = str_replace_all(category, "Porous Contaminated", "Porous_Contaminated") 
                            %>% str_replace_all("Subsurface Saline", "Subsurface_saline"))
data <- data %>% as.data.frame() %>% arrange("Samples")


spe<-data.frame(data, check.names=FALSE) 
spe.num<-spe[,3:ncol(spe)]

Sample <- spe[,2]
Category2 <- as.factor(spe[,1])



dist.matrix <- vegdist(spe.num,method = "manhattan")

perm_model <- adonis(spe.num~Category2, permutations = 4999)

pairwise_fun <- adonis.pair(dist.matrix, Category2, nper = 4999, corr.method = "fdr")

write.csv(pairwise_fun, file = paste("outputs/functional_permanova-pairwise",d,".csv"))

#Permanova pairwise for the entire community - taxonomic

data2 <- read.csv("inputs/genus_groundwater_annotated_matrix_2021_12_16.csv", check.names=FALSE)
spe<-data.frame(data2, check.names=FALSE) %>% arrange("samples")
spe.num<-spe[,5:ncol(spe)]

Sample <- data2[,1]
Category <- as.factor(data2[,4])

dist.matrix <- vegdist(spe.num,method = "manhattan", binary = F)

permanova <- adonis(dist.matrix ~ Category, permutations = 4999)
summary(permanova)
permanova

pairwise <- adonis.pair(dist.matrix, Category, nper = 4999, corr.method = "fdr")

write.csv(pairwise, file = paste("outputs/taxonomic_permanova-pairwise",d,".csv"))



##### TAXONOMIC PERANOVA #####
data2 <- read.csv("inputs/genus_groundwater_annotated_matrix_2021_12_16.csv", check.names=FALSE)
spe<-data.frame(data2, check.names=FALSE) %>% arrange("samples")
spe.num<-spe[,5:ncol(spe)]

Sample <- data2[,1]
Category <- as.factor(data2[,4])

# Calculate the richness and diversity to analysis

N0 <- rowSums(spe.num > 0)
H <- diversity(spe.num)
N1 <- exp(H)

#Running peranova for richness
tax_rich <- aovp(N0~Category, data = data2)

summary(tax_rich)
tax_rich$coefficients

#Pairwise for richness
tN0<-data.frame(Sample,Category,N0)
tN0$Category<-factor(tN0$Category)

dist.matrix <- vegdist(tN0$N0,method = "manhattan", binary = F)
pairwise_rich <- adonis.pair(dist.matrix, Category, nper = 4999, corr.method = "fdr")

write.csv(pairwise_rich, file = paste("outputs/taxonomic_richness_permanova-pairwise",d,".csv"))
#Running peranova for diversity
tax_div <- aovp(N1~Category, data = data2)

summary(tax_div)
tax_div$coefficients

#Pairwise for diversity
tN1<-data.frame(Sample,Category,N1) #Shannon diversity number
tN1$Category<-factor(tN1$Category)

dist.matrix <- vegdist(tN1$N1, method = "manhattan", binary = F)
pairwise_div <- adonis.pair(dist.matrix, Category, nper = 4999, corr.method = "fdr")

write.csv(pairwise_div, file = paste("outputs/taxonomic_diversity_permanova-pairwise",d,".csv"))

###### FUNCTIONAL PERANOVA #####
data <- read.csv("inputs/functional_geral_matrix_lvl4_2021_12_16.csv", check.names=FALSE) %>% as_tibble()

spe<-data.frame(data, check.names=FALSE) 
spe.num<-spe[,3:ncol(spe)]

Sample <- spe[,2]
Category2 <- as.factor(spe[,1])

# Calculate the richness and diversity to analysis

N0 <- rowSums(spe.num > 0)
H <- diversity(spe.num)
N1 <- exp(H)

#Running peranova for richness
fun_rich <- aovp(N0~Category2, data = data)

summary(fun_rich)

#Pairwise for richness
tN0<-data.frame(Sample,Category2,N0) #Shannon diversity number
tN0$Category2<-factor(tN0$Category2)

dist.matrix <- vegdist(tN0$N0, method = "manhattan", binary = F)
pairwise_rich <- adonis.pair(dist.matrix, Category2, nper = 4999, corr.method = "fdr")

write.csv(pairwise_rich, file = paste("outputs/functional_richness_permanova-pairwise",d,".csv"))

#Running peranova for diversity
fun_div <- aovp(N1~Category2, data = data)

summary(fun_div)

#Pairwise for diversity
tN1<-data.frame(Sample,Category2,N1) #Shannon diversity number
tN1$Category<-factor(tN1$Category2)

dist.matrix <- vegdist(tN1$N1, method = "manhattan", binary = F)
pairwise_div <- adonis.pair(dist.matrix, Category2, nper = 4999, corr.method = "fdr")

write.csv(pairwise_div, file = paste("outputs/functional_diversity_permanova-pairwise",d,".csv"))
