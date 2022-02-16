library(ggplot2)
library(vegan)
library(RColorBrewer)

#### SET WORK DIRECTORY ####
setwd("~/Documentos/Felipe/Functional_groundwater")

set.seed(1000)

#Save the system date to organize your outputs
d <- Sys.Date() %>% str_replace_all("-","_")


############Stacked Bar Taxonomico#############################

#gens_abundance <- read.csv("abundance_gens0.csv", sep = "\t", header = T)
gensdata <- read.csv("inputs/phyla_groundwater_annotated_matrix_2021-03-08.csv", sep = ",", header = T)
gensdata_2 <- gensdata[,-c(2)]
rbind(gensdata_2, c("Average", "Average",colSums(select_if((gensdata_2), is.numeric))/118))


data_num <-select_if((gensdata_2), is.numeric)


#Creating a vector with abundant phyla names, so you cant summarize the rare microbiota
#in a column named "others"

#Creating vector. The "1/100" represents the cutoff to define rare and bonafide taxa
abundants <- data_num %>% gather() %>% group_by(key) %>% 
  summarise(taxa_mean = mean(value)) %>% 
  filter(taxa_mean > 1/100) %>% pull(key)


#Create a dataframe with only abundants
abundant.class <- dplyr::select(gensdata, abundants)

#Create a dataframe with only others
#Only numeric columns, because we are sum all of them in one column
others <- dplyr::select(gensdata[,-(1:3)], -abundants)


# Merging abundant and others
abundant.class$Others <- rowSums(others)

##Ordering dataframe by average of taxa
#Create a row with average of each taxa. Replace 115 by the number of rows that you have
abundant.class <- rbind(abundant.class, colSums(select_if((abundant.class), is.numeric))/115)
#Transpose, so you can ord by the new column created with average values
abundant.class <- t(abundant.class)
abundant.class_ord <- abundant.class[order(abundant.class[,116], decreasing = T),]
#Transpose back
abundant.class <- t(abundant.class_ord)
#Convert to dataframe and removing the row with average values
abundant.class <- as.data.frame(abundant.class[-116,])

#Mergind with sample information
abundant.class$category <- gensdata$level_3
abundant.class$samples <- gensdata$samples

#Ordering the catergories (not mandatory)
abundant.class <- abundant.class[order(abundant.class$category),]

#Melt data to plot
gens_final= melt(abundant.class)

###Create a pie with the colors that you will need to the plot with "scale_fill_manual". I need 35.
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'seq',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
colors=col_vector[sample(1:length(col_vector),14)]


tax_stack <- ggplot(gens_final, aes(x=samples, y=value, fill = variable)) + 
  geom_bar(stat="identity",width=1)+
  labs(title = "Taxonomic Structure", x = "Samples", y = "Relative Abundance", fill = "Phyla")+ 
  guides(fill=guide_legend(reverse=FALSE))+
  scale_fill_manual(values=colors)+
  scale_color_discrete(name = "Phyla")+
  facet_grid(cols = vars(category), space = "free", scales = "free")+
  theme_bw() + 
  theme(axis.text.x = element_blank())+
  theme(strip.text = element_text(face="bold", size=14),
        strip.background = element_rect(fill="white", colour="black",size=1))+
  theme(axis.text.y = element_text(size=30))+
  theme(axis.title.x = element_text(size=30), axis.title.y = element_text(size=30), plot.title = element_text(size = 30))+
  theme(axis.line = element_line())+
  theme(axis.ticks = element_blank())+
  theme(panel.border = element_blank())+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size=.5, color="darkgrey"),
        panel.grid.minor.y = element_blank())+
  theme(plot.tag = element_text(size = 35, face = "bold"))+
  theme(axis.line.x=element_blank())+
  theme(axis.line.y=element_blank())+
  theme(axis.line.y=element_blank())+
  theme(legend.title=element_text(size=35, face = "bold"))+
  theme(legend.text=element_text(size=32))+
  theme(legend.justification=c(0.5,0.5), legend.position=c("right"))


tax_stack_leg <- get_legend(tax_stack)

tax_stack <- tax_stack + theme(legend.position = "none")

tax_stack

ggsave(file=paste('outputs/plot_taxonomic_ord_abund_groundwater_final',d,'.png', sep = ''),width = 35, height = 16)
ggsave(file=paste('outputs/plot_taxonomic_ord_abund_groundwater_final',d,'.svg', sep = ''),width = 35, height = 16)
ggsave(file=paste('outputs/plot_taxonomic_ord_abund_groundwater_final',d,'.pdf', sep = ''),width = 35, height = 16)
