import_HO <- function(data){ 
library(dplyr)
# hoch <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/Hochstetter_arthropod_biomass.txt", header= T, sep="\t")

names(hoch)[names(hoch) == "Year"] <- "Period_Year"
names(hoch)[names(hoch) == "Size"] <- "Arthropod_Length"
names(hoch)[names(hoch) == "Order"] <- "Arthropod_Order"
names(hoch)[names(hoch) == "Plot"] <- "Habitat"
names(hoch)[names(hoch) == "DOY"] <- "Period_Day"
hoch$Field_site<- "Hochstetter"
hoch$nb_of_trap<- 8
hoch$trap_days <- 7
hoch$Arthropod_Biomass<- NA
hoch$Habitat <- as.character(hoch$Habitat)

#exclude unwanted taxa
hoch<-filter(hoch, Arthropod_Order!="Lepidoptera")
hoch<-filter(hoch, Arthropod_Order!="Acari_collembola")
hoch<-filter(hoch, Arthropod_Order!="Psychophora") # lepido
hoch<-filter(hoch, Arthropod_Order!="Thysanoptera") # pas d'équation, seulement 1 individu
# apply equations
funDip <- function(L){0.009*(L)^2.797}
funAra <- function(L){0.028*(L)^2.761}
funHym <- function(L){0.014*(L)^2.487}
funCat<- function(L){0.0011*exp(0.1495*L)}
funHem<- function(L){0.01*(L)^2.858}
funTip<- function(L){0.004*(L)^2.609}

#extract individual biomasses from ASDN data

arthro_all <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/raw/ARTHROPOD_ALL.txt", header = TRUE)
df <- data.frame(matrix(ncol=5, nrow=0))
x <- c("Order","Family", "Genus", "moy_ind", "med_ind")
colnames(df) <- x


# biomasse sac d'oeufs
egg <- arthro_all[which(arthro_all$Stage == "Egg sac"),]
egg<- dplyr::filter(egg, !is.na(Arthropod_Biomass))
egg<-dplyr::filter(egg, !is.na(Arthropod_Count))
egg <- dplyr::filter(egg, Arthropod_Biomass > 0)
egg$masse_ind <- egg$Arthropod_Biomass/egg$Arthropod_Count


m_egg<- lm(data=egg, masse_ind~Arthropod_Length)
summary(m_egg)
#formula = -1.53073+0.70576*L
funEgg <- function(L){-1.53073+0.70576*L}

hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Aranea")]<-funAra(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Aranea")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Aranea_spiderlings")]<-funAra(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Aranea_spiderlings")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Diptera")]<-funDip(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Diptera")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Hymenoptera")]<-funHym(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Hymenoptera")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Caterpillar")]<-funCat(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Caterpillar")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Lepidoptera_caterpillar")]<-funCat(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Lepidoptera_caterpillar")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Hemiptera")]<-funHem(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Hemiptera")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Tipulidae")]<-funTip(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Tipulidae")])
hoch$Arthropod_Biomass[which(hoch$Arthropod_Order=="Aranea_eggsac")]<-funEgg(hoch$Arthropod_Length[which(hoch$Arthropod_Order=="Aranea_eggsac")])


source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
dfHO<- daily_biomass(data=hoch)
write.table(dfHO, file = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/dfHO_pre_conv.txt")
source('~/Desktop/MAITRISE/scripts/CAL_conv_fun.R')
dfHO<-CAL_conv(dfHO)
return(dfHO)

}
