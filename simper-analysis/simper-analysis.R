library(vegan)
library(dplyr)

#### SET WORK DIRECTORY ####
setwd("~/Documentos/Felipe/Functional_groundwater")
#setwd("~/Documentos/Felipe/BTS/Functional tables/")

#Save the system date to organize your outputs
d <- Sys.Date() %>% str_replace_all("-","_")


###### Functional Simper #####
data <- read.csv("inputs/functional_geral_matrix_lvl4_2021_12_15.csv") %>% as_tibble()
colnames(data)[2] <- "Samples"
data <- column_to_rownames(data, var = "Samples")
agrupamento <- data[,1]


fun_comunidade <- data[,-1]
fun_comunidade[1:5,1:5]

aquifer_taxa <- with(fun_comunidade, simper(fun_comunidade, agrupamento, permutations = 1000))
aquifer_taxa
summary(aquifer_taxa)

summ_simper <- summary(aquifer_taxa)
#Saving outputs
write.csv(file = "outputs/simper_results_Porous_Contaminated_Karst_Porous.csv", x = summ_simper[["Porous Contaminated_Karst-Porous"]])
write.csv(file = "outputs/simper_results_Porous_Contaminated_Mine.csv", x = summ_simper[["Porous Contaminated_Mine"]])
write.csv(file = "outputs/simper_results_Porous_Contaminated_Subsurface_saline.csv", x = summ_simper[["Porous Contaminated_Subsurface Saline"]])x
write.csv(file = "outputs/simper_results_Porous_Contaminated_Porous.csv", x = summ_simper[["Porous Contaminated_Porous"]])
write.csv(file = "outputs/simper_results_Porous_Contaminated_Geyser.csv", x = summ_simper[["Porous Contaminated_Geyser"]])

write.csv(file = "outputs/simper_results_Karst_Porous_Mine.csv", x = summ_simper[["Karst-Porous_Mine"]])
write.csv(file = "outputs/simper_results_Karst_Porous_Subsurface_saline.csv", x = summ_simper[["Karst-Porous_Subsurface Saline"]])x
write.csv(file = "outputs/simper_results_Karst_Porous_Porous.csv", x = summ_simper[["Karst-Porous_Porous"]])
write.csv(file = "outputs/simper_results_Karst_Porous_Geyser.csv", x = summ_simper[["Karst-Porous_Geyser"]])

write.csv(file = "outputs/simper_results_Mine_Subsurface_saline.csv", x = summ_simper[["Mine_Subsurface Saline"]])
write.csv(file = "outputs/simper_results_Mine_Porous.csv", x = summ_simper[["Mine_Porous"]])
write.csv(file = "outputs/simper_results_Mine_Geyser.csv", x = summ_simper[["Mine_Geyser"]])

write.csv(file = "outputs/simper_results_Subsurface_saline_Porous.csv", x = summ_simper[["Subsurface Saline_Porous"]])
write.csv(file = "outputs/simper_results_Subsurface_saline_Geyser.csv", x = summ_simper[["Subsurface Saline_Geyser"]])

write.csv(file = "outputs/simper_results_Porous_Geyser.csv", x = summ_simper[["Porous_Geyser"]])


PCxKP<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Porous_Contaminated_Karst_Porous.csv")
PCxMine<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Porous_Contaminated_Mine.csv")
PCxSS<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Porous_Contaminated_Subsurface_saline.csv")
PCxP<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Porous_Contaminated_Porous.csv")
PCxGey<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Porous_Contaminated_Geyser.csv")
KPxMine<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Karst_Porous_Mine.csv")
KPxSS<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Karst_Porous_Subsurface_saline.csv")
KPxP<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Karst_Porous_Porous.csv")
KPxGey<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Karst_Porous_Geyser.csv")
MinexSS<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Mine_Subsurface_saline.csv")
MinexP<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Mine_Porous.csv")
MinexGey<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Mine_Geyser.csv")
SSxP<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Subsurface_saline_Porous.csv")
SSxGey<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Subsurface_saline_Geyser.csv")
PxGey<-read.csv("outputs/functional_simper_results_16_12_2021/simper_results_Porous_Geyser.csv")


PCxKP<-PCxKP[order(PCxKP$X),]
PCxMine<-PCxMine[order(PCxMine$X),]
PCxSS<-PCxSS[order(PCxSS$X),]
PCxP<-PCxP[order(PCxP$X),]
PCxGey<-PCxGey[order(PCxGey$X),]
KPxMine<-KPxMine[order(KPxMine$X),]
KPxSS<-KPxSS[order(KPxSS$X),]
KPxP<-KPxP[order(KPxP$X),]
KPxGey<-KPxGey[order(KPxGey$X),]
MinexSS<-MinexSS[order(MinexSS$X),]
MinexP<-MinexP[order(MinexP$X),]
MinexGey <-MinexGey[order(MinexGey$X),]
SSxP<-SSxP[order(SSxP$X),]
SSxGey<-SSxGey[order(SSxGey$X),]
PxGey<-PxGey[order(PxGey$X),]

#Checking if the order is correct
KPxSS[350,1] == PxGey[350,1]

#Binding all average columns in one data.frame
avg <-cbind(PCxKP[c(1,2)],PCxMine[2],PCxSS[2], PCxP[2],PCxGey[2],KPxMine[2],KPxSS[2],KPxP[2],
            KPxGey[2],MinexSS[2], MinexP[2], MinexGey[2], SSxP[2], SSxGey[2], PxGey[2])
#Changing colnames
colnames(avg) <- c("Average Groups Contribution to dissimilarity","Porous Contaminated x Karst-Porous","Porous Contaminated x Mine","Porous Contaminated x Subsurface saline",
                   "Porous Contaminated x Porous","Porous Contaminated x Geyser","Karst-Porous x Mine","Karst-Porous x Subsurface saline",
                   "Karst-Porous x Porous","Karst-Porous x Geyser","Mine x Subsurface saline", "Mine x Porous", "Mine x Geyser", "Subsurface saline x Porous", 
                   "Subsurface saline x Geyser", "Porous x Geyser")
#Saving matrix
write.csv(avg, file = paste("outputs/simper-functional-avg_cont_",d,".csv"), row.names = F)


##### Taxonomic simper #####

data <-  read.csv("inputs/genus_groundwater_annotated_matrix_2021_12_16.csv")
data <- data %>% mutate(category = str_replace_all(category, "Porous_Contaminated", "Porous Contaminated") 
                        %>% str_replace_all("Subsurface_saline", "Subsurface saline"))

comunidade <- select_if((data), is.numeric)
agrupamento <- data[,"category"]
aquifer_taxa <- with(comunidade, simper(comunidade, agrupamento, permutations = 1000))
aquifer_taxa
summary(aquifer_taxa)

summ_simper <- summary(aquifer_taxa)
#Saving outputs
write.csv(file = "outputs/simper_results_Porous_Contaminated_Karst_Porous.csv", x = summ_simper[["Porous Contaminated_Karst-Porous"]])
write.csv(file = "outputs/simper_results_Porous_Contaminated_Mine.csv", x = summ_simper[["Porous Contaminated_Mine"]])
write.csv(file = "outputs/simper_results_Porous_Contaminated_Subsurface_saline.csv", x = summ_simper[["Porous Contaminated_Subsurface saline"]])
write.csv(file = "outputs/simper_results_Porous_Contaminated_Porous.csv", x = summ_simper[["Porous Contaminated_Porous"]])
write.csv(file = "outputs/simper_results_Porous_Contaminated_Geyser.csv", x = summ_simper[["Porous Contaminated_Geyser"]])

write.csv(file = "outputs/simper_results_Karst_Porous_Geyser.csv", x = summ_simper[["Karst-Porous_Geyser"]])

write.csv(file = "outputs/simper_results_Mine_Subsurface_saline.csv", x = summ_simper[["Mine_Subsurface saline"]])
write.csv(file = "outputs/simper_results_Mine_Porous.csv", x = summ_simper[["Mine_Porous"]])
write.csv(file = "outputs/simper_results_Karst_Porous_Mine.csv", x = summ_simper[["Karst-Porous_Mine"]])
write.csv(file = "outputs/simper_results_Mine_Geyser.csv", x = summ_simper[["Mine_Geyser"]])

write.csv(file = "outputs/simper_results_Subsurface_saline_Porous.csv", x = summ_simper[["Subsurface saline_Porous"]])
write.csv(file = "outputs/simper_results_Karst_Porous_Subsurface_saline.csv", x = summ_simper[["Karst-Porous_Subsurface saline"]])
write.csv(file = "outputs/simper_results_Subsurface_saline_Geyser.csv", x = summ_simper[["Subsurface saline_Geyser"]])

write.csv(file = "outputs/simper_results_Porous_Geyser.csv", x = summ_simper[["Porous_Geyser"]])
write.csv(file = "outputs/simper_results_Karst_Porous_Porous.csv", x = summ_simper[["Karst-Porous_Porous"]])

PCxKP<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Porous_Contaminated_Karst_Porous.csv")
PCxMine<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Porous_Contaminated_Mine.csv")
PCxSS<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Porous_Contaminated_Subsurface_saline.csv")
PCxP<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Porous_Contaminated_Porous.csv")
PCxGey<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Porous_Contaminated_Geyser.csv")
KPxMine<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Karst_Porous_Mine.csv")
KPxSS<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Karst_Porous_Subsurface_saline.csv")
KPxP<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Karst_Porous_Porous.csv")
KPxGey<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Karst_Porous_Geyser.csv")
MinexSS<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Mine_Subsurface_saline.csv")
MinexP<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Mine_Porous.csv")
MinexGey<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Mine_Geyser.csv")
SSxP<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Subsurface_saline_Porous.csv")
SSxGey<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Subsurface_saline_Geyser.csv")
PxGey<-read.csv("outputs/taxonomic_simper_results_16_12_2021/simper_results_Porous_Geyser.csv")


PCxKP<-PCxKP[order(PCxKP$X),]
PCxMine<-PCxMine[order(PCxMine$X),]
PCxSS<-PCxSS[order(PCxSS$X),]
PCxP<-PCxP[order(PCxP$X),]
PCxGey<-PCxGey[order(PCxGey$X),]
KPxMine<-KPxMine[order(KPxMine$X),]
KPxSS<-KPxSS[order(KPxSS$X),]
KPxP<-KPxP[order(KPxP$X),]
KPxGey<-KPxGey[order(KPxGey$X),]
MinexSS<-MinexSS[order(MinexSS$X),]
MinexP<-MinexP[order(MinexP$X),]
MinexGey <-MinexGey[order(MinexGey$X),]
SSxP<-SSxP[order(SSxP$X),]
SSxGey<-SSxGey[order(SSxGey$X),]
PxGey<-PxGey[order(PxGey$X),]

#Checking if the order is correct
KPxSS[350,1] == PxGey[350,1]

#Binding all average columns in one data.frame
avg <-cbind(PCxKP[c(1,2)],PCxMine[2],PCxSS[2], PCxP[2],PCxGey[2],KPxMine[2],KPxSS[2],KPxP[2],
            KPxGey[2],MinexSS[2], MinexP[2], MinexGey[2], SSxP[2], SSxGey[2], PxGey[2])
#Changing colnames
colnames(avg) <- c("Average Groups Contribution to dissimilarity","Porous Contaminated x Karst-Porous","Porous Contaminated x Mine","Porous Contaminated x Subsurface saline",
                   "Porous Contaminated x Porous","Porous Contaminated x Geyser","Karst-Porous x Mine","Karst-Porous x Subsurface saline",
                   "Karst-Porous x Porous","Karst-Porous x Geyser","Mine x Subsurface saline", "Mine x Porous", "Mine x Geyser", "Subsurface saline x Porous", 
                   "Subsurface saline x Geyser", "Porous x Geyser")
#Saving matrix
write.csv(avg, file = paste("outputs/simper-taxonomic-avg_cont_",d,".csv"), row.names = F)
