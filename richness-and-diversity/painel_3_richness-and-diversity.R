setwd("~/Documentos/Felipe/Functional_groundwater/")

##### Figure 3 ####
library(vegan)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(ggpubr)
library(rstatix)

d <- Sys.Date() %>% str_replace_all("-","_")

data2 <- read.csv("inputs/genus_groundwater_annotated_matrix_2021_07_29.csv", check.names=FALSE) %>% 
  mutate(level_3 = str_replace_all(level_3, "Porous_Contaminated", "Porous Contaminated")  %>%
           str_replace_all("Subsurface_saline", "Subsurface saline"))
spe<-data.frame(data2, check.names=FALSE)
spe.num<-spe[,5:ncol(spe)]

Sample <- data2[,1]
Category <- data2[,4]

#Richness
N0 <- rowSums(spe.num > 0)

#Shannon diversity
H <- diversity(spe.num)
N1 <- exp(H)

#Combine with other information and summarize by aquifer type
tN0<-data.frame(Sample,Category,N0)
tN0$Category<-factor(tN0$Category)

tN0 %>% group_by(Category) %>% summarise(mean(N0))

tN1<-data.frame(Sample,Category,N1) #Shannon diversity number
tN1$Category<-factor(tN1$Category)

tN1 %>% group_by(Category) %>% summarise(mean(N1))

tN0$Category <- factor(tN0$Category, levels=c("Porous", "Mine", "Karst-Porous", "Subsurface saline", "Porous Contaminated", "Geyser"))
my_comparisons <- list(c("Karst-Porous","Porous"), c("Mine","Subsurface saline"), c("Mine","Porous Contaminated"), c("Porous","Subsurface saline"), c("Porous","Porous Contaminated"))
#Plot genus richness
p3 <- ggplot(tN0, aes(x = Category, y=N0))+
  geom_boxplot(fill = "#808080", width = 0.5)+
  guides(fill=NULL)+ 
  theme_bw()+
  labs(title = NULL, x = NULL, y = "Number of Microbial \nGenera")+
  theme(axis.text.x = element_text(size = 21, angle = 45, hjust = 1))+
  theme(axis.title.y = element_text(size=21,face="bold"))+
  theme(axis.text.y = element_text(size=21))+
  theme(panel.border = element_blank())+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.y = element_blank())+
  theme(axis.line = element_line())+
  theme(panel.grid.major.y = element_blank())#+
  #stat_compare_means(method = "anova")+
  #stat_compare_means(comparisons = my_comparisons)
p3

#To visualize the comparisions and pvalue, exclude the "#" from the last two lines

tN1$Category <- factor(tN1$Category, levels=c("Porous", "Mine", "Karst-Porous", "Subsurface saline", "Porous Contaminated", "Geyser"))
#Plot genus diveristy
p4 <- ggplot(tN1, aes(x = Category, y=N1))+
  geom_boxplot(fill = "#808080", width = 0.5)+
  guides(fill=NULL)+ 
  theme_bw()+
  labs(title = NULL, x = NULL, y = "Shannon Diversity \nIndex (Genus Level)")+
  theme(axis.text.x = element_text(size = 21, angle = 45, hjust = 1))+
  theme(axis.title.y = element_text(size=21,face="bold"))+
  theme(axis.text.y = element_text(size=21))+
  theme(panel.border = element_blank())+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.y = element_blank())+
  theme(axis.line = element_line())+
  theme(panel.grid.major.y = element_blank())
p4

plot_grid(p3, p4)

ggsave(filename = paste("outputs/taxonomic_panel_1_",d,".svg", sep = ""), width = 15, height = 7, limitsize = FALSE)
ggsave(filename = paste("outputs/taxonomic_panel_1_",d,".png", sep = ""), width = 15, height = 7, limitsize = FALSE)


#### Functional

data <- read.csv("inputs/functional_geral_matrix_lvl4_2021_12_07.csv", check.names=FALSE) %>% as_tibble()
spe<-data.frame(data, check.names=FALSE)
spe.num<-spe[,3:ncol(spe)]

#Richness
N0 <- rowSums(spe.num > 0)
#Diversity
H <- diversity(spe.num)
N1 <- exp(H)

Sample <- data[,2]
Category <- data[,1]

tN0<-data.frame(Sample,Category,N0)
tN0$Category<-factor(tN0$category)

tN0 %>% group_by(Category) %>% summarise(mean(N0))

tN1<-data.frame(Sample,Category,N1) #Shannon diversity number
tN1$Category<-factor(tN1$category)

tN1 %>% group_by(Category) %>% summarise(mean(N1))

tN0$Category <- factor(tN0$category , levels=c("Porous","Mine", "Karst-Porous", "Subsurface Saline", "Porous Contaminated", "Geyser"))

#Plot functions richness
p1 <- ggplot(tN0, aes(x = Category, y=N0))+
  geom_boxplot(fill = "#808080", width = 0.5)+
  #geom_dotplot(binaxis='y', stackdir='center',position=position_dodge(0.75))+
  guides()+ 
  #scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values=c("#808080")) +
  theme_bw()+
  labs(title = NULL, x = NULL, y = "Number of functions (SEED Level 4)")+
  theme(axis.text.x = element_text(size = 21, angle = 45, hjust = 1))+
  theme(axis.title.y = element_text(size=21,face="bold"))+
  theme(axis.text.y = element_text(size=21))+
  theme(panel.border = element_blank())+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.y = element_blank())+
  theme(axis.line = element_line())+
  theme(panel.grid.major.y = element_blank())
p1


tN1$Category <- factor(tN1$category , levels=c("Porous","Mine", "Karst-Porous", "Subsurface Saline", "Porous Contaminated", "Geyser"))
#Plot functional diversity
p2 <- ggplot(tN1, aes(x = Category, y=N1))+
  geom_boxplot(fill = "#808080", width = 0.5)+
  #geom_dotplot(binaxis='y', stackdir='center',position=position_dodge(0.75))+
  guides(fill=NULL)+ 
  #scale_fill_brewer(palette="Paired")+
  #scale_fill_manual(values=c(""#43CD80","#9400D3","steelblue3","#DEB887")) +
  theme_bw()+
  labs(title = NULL, x = NULL, y = "Shannon Diversity Index \n(SEED Level 4 Level)")+
  theme(axis.text.x = element_text(size = 21, angle = 45, hjust = 1))+
  theme(axis.title.y = element_text(size=21,face="bold"))+
  theme(axis.text.y = element_text(size=21))+
  theme(panel.border = element_blank())+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.minor.y = element_blank())+
  theme(axis.line = element_line())+
  theme(panel.grid.major.y = element_blank())
p2

plot_grid(p1, p2)

ggsave(filename = paste("outputs/functional_panel_1_",d,".svg", sep = ""), width = 15, height = 7, limitsize = FALSE)
ggsave(filename = paste("outputs/functional_panel_1_",d,".png", sep = ""), width = 15, height = 7, limitsize = FALSE)


plot_grid(p3, p4, p1, p2, ncol = 2)
ggsave(filename = paste("outputs/panel_1_",d,".svg", sep = ""), width = 16, height = 12, limitsize = FALSE)
ggsave(filename = paste("outputs/panel_1_",d,".png", sep = ""), width = 16, height = 12, limitsize = FALSE)

