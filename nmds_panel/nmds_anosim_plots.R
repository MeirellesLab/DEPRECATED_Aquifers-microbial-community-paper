#Libraries
library(vegan)
library(dplyr)
library(ggplot2)
library(cowplot)

##### SET WORK DIRECTORY ####
setwd("~/Documentos/Felipe/Functional_groundwater")


#Set seed
set.seed(1000)
#Save the system date to organize your outputs
d <- Sys.Date() %>% str_replace_all("-","_")

############### nMDS funcional ####################

#Loading data
data <- read.csv("inputs/functional_geral_matrix_lvl4_2021_12_16.csv") %>% as_tibble()

#Data wrangling
colnames(data)[2] <- "Samples"
data_num <- data[,-c(1,2)]
category <- as.factor(data$category)

#MDS
ord <- metaMDS(data_num, distance = "bray", trymax = 1000)

#Combining with other columns
ord_df <- cbind(data$Samples,category,data.frame(ord$points))
colnames(ord_df) <- cbind("Samples","Category", "MDS1", "MDS2")

attach(ord_df)

#Running anosim
danosim <- anosim(data_num,data$category, permutations = 99999)

plot_fun <- ggplot(ord_df, aes(x=MDS1, y=MDS2, color=Category, mapping = Samples)) + 
  geom_point(size = 4) +
  theme_bw()+
  labs(title = "Functional Level 4", tag = "B")+
  theme(plot.title = element_text(face = "bold", size = 32), plot.tag = element_text(face = "bold", size = 36))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(colour = "black", size=1))+
  theme(legend.key=element_rect(fill='white'))+
  theme(legend.key = element_rect(colour = "white"))+
  scale_colour_manual(values=c("#43CD80","#9400D3","#c2c212", "#3eb5bf", "#7e3c03", "#0f0476"))+ 
  #O comando acima descrimina as cores a serem utilizadas 
  #O NUMERO DE CODIGOS DEVE SER IDENTICO AO NUMERO DE PONTOS!!!
  #EM ANEXO UMA TABELA COM OS NOMES DAS CORES.
  theme(legend.text=element_text(size=30))+
  theme(axis.title.x = element_text(face="bold", size=32))+
  theme(axis.title.y = element_text(face="bold", size=32))+
  theme(axis.text.x = element_text(size=32,color="black"))+
  theme(axis.text.y = element_text(size=32,color="black"))+
  theme(axis.line.x=element_blank())+
  theme(axis.line.y=element_blank())+
  annotate("text", x = max(ord_df$MDS1)-0.21, y = max(ord_df$MDS2)-0.05, label = paste("Stress=",round(ord$stress, digits=3)), size =8)+
  annotate("text", x=max(MDS1)-0.25, y=max(MDS2)-0.13, label = paste("R²=",round(danosim$statistic, digits = 3)), size = 8)+
  annotate("text", x=max(MDS1)-0.23, y=max(MDS2)-0.20, label = paste("pvalue < 0.001"), size = 8)+
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

plot_fun

ggsave(plot = plot_fun, file=paste("outputs/output_nMDS_funcional_lvl3_modified_",d,".jpg", sep = ""), width=10, height=10)
ggsave(plot = plot_fun, file=paste("outputs/output_nMDS_funcional_lvl3_modified_",d,".svg", sep = ""), width=10, height=10)
ggsave(plot = plot_fun, file=paste("outputs/output_nMDS_funcional_lvl3_modified_",d,".png", sep = ""), width=10, height = 10)

#library(plotly)

#ggplotly(plot_fun)
############# nMDS taxonômico ################

data <- read.csv("inputs/genus_groundwater_annotated_matrix_2021_12_16.csv", sep = ",", header = T)
samples <- data2[,1]
data_num <- select_if(data2, is.numeric)

data <- data %>% mutate(level_3 = str_replace_all(category, "Porous_Contaminated", "Porous Contaminated") 
                        %>% str_replace_all("Subsurface_saline", "Subsurface Saline"))
category <- as.factor(data$category)
ord <- metaMDS(data_num, distance = "bray", trymax = 1000)
ord_df <- cbind(samples,data$category,data.frame(ord$points))
colnames(ord_df) <- cbind("Samples", "Category", "MDS1","MDS2")
attach(ord_df)

danosim <- anosim(data_num,category, permutations = 99999)

plot_tax <- ggplot(ord_df, aes(x=MDS1, y=MDS2, color=category)) + 
  geom_point(size = 4) +
  theme_bw()+
  labs(title = "Genus", tag = "A", color = "Category")+
  theme(plot.title = element_text(face = "bold", size = 32), plot.tag = element_text(face = "bold", size = 36))+
  theme(legend.position = "right", legend.title = element_text(size = 30, face = "bold"))+
  theme(panel.border = element_rect(colour = "black", size=1))+
  theme(legend.key=element_rect(fill='white'))+
  theme(legend.key = element_rect(colour = "white"))+
  scale_colour_manual(values=c("#43CD80","#9400D3","#c2c212", "#3eb5bf", "#7e3c03", "#0f0476"))+ 
  theme(legend.text=element_text(size=32))+
  theme(axis.title.x = element_text(face="bold", size=32))+
  theme(axis.title.y = element_text(face="bold", size=32))+
  theme(axis.text.x = element_text(size=32,color="black"))+
  theme(axis.text.y = element_text(size=32,color="black"))+
  theme(axis.line.x=element_blank())+
  theme(axis.line.y=element_blank())+
  annotate("text", x = max(MDS1)-0.78, y = max(MDS2)-0.01, label = paste("Stress=",round(ord$stress, digits=3)), size = 8)+
  annotate("text", x=max(MDS1)-0.75, y=max(MDS2)-0.45, label = paste("R²=",round(danosim$statistic, digits = 3)), size = 8)+
  annotate("text", x=max(MDS1)-0.85, y=max(MDS2)-0.84, label = paste("pvalue < 0.001"), size = 8)+
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

plot_tax

legend_nmds <- get_legend(plot_tax)

plot_tax <- plot_tax + theme(legend.position = "none")

plot_tax

ggsave(plot = plot_tax,file=paste("outputs/output_nMDS_family_modified_",d,".jpg", sep = ""), width=10, height=10)
ggsave(plot = plot_tax,file=paste("outputs/output_nMDS_family_modified_",d,".svg"), width=10, height=10)
ggsave(plot = plot_tax,file=paste("outputs/output_nMDS_family_modified_",d,".png"), width = 10, height = 10)

##Plotting panel##
plot_grid(plot_tax, plot_fun, legend_nmds, align = "h", ncol = 3, greedy = T)
ggsave(file=paste("outputs/new_nmds_groundwater_panel",d,".jpg", sep = ""), width=29, height=10)
ggsave(file=paste("outputs/new_nmds_groundwater_panel",d,".svg", sep = ""), width=29, height=10)
ggsave(file=paste("outputs/new_nmds_groundwater_panel",d,".png", sep = ""), width=29, height=10)
