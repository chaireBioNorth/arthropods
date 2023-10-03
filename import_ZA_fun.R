import_ZA <- function(data){
  #data<-ZA
  zackenberg <- data
#renommer colonnes pour que ça concorde avec ARTHRO_ALL
names(zackenberg)[names(zackenberg) == "Plot.ID"] <- "Area_Name"
names(zackenberg)[names(zackenberg) == "Order"] <- "Arthropod_Order"
names(zackenberg)[names(zackenberg) == "Family"] <- "Arthropod_Family"
names(zackenberg)[names(zackenberg) == "IDtrap"] <- "Trap_number"
names(zackenberg)[names(zackenberg) == "Abundance"] <- "Arthropod_Count"
names(zackenberg)[names(zackenberg) == "Longueur.moyenne.x"] <- "Arthropod_Length"
names(zackenberg)[names(zackenberg) == "biomasse"] <- "Arthropod_Biomass"

#enlever lignes qui ne serviront pas
zackenberg <- zackenberg[,-c(10:12, 14:15)]

#retirer 1996 et 1997 (trop longues périodes de trappe)
zackenberg<- dplyr::filter(zackenberg, Period_Year != 1997 & Period_Year != 1996)

#rajouter les colonnes manquantes
zackenberg$ID <- 1:nrow(zackenberg)
zackenberg$Field_site <- "Zackenberg"

zackenberg$Sample_Location <- "Denmark"

zackenberg$Habitat <- zackenberg$Area_Name


# ..........sélection des habitats ----

zackenberg <- dplyr::filter(zackenberg, Habitat == "Art2" |Habitat == "Art3" |Habitat == "Art4"  )
hum <- dplyr::filter(zackenberg, Habitat == "Art2")
hum$Habitat <- "Humid"

art3 <- dplyr::filter(zackenberg, Habitat == "Art3")
art3$Trap_number <- (paste(art3$Trap_number, "3", sep=""))
art4 <- dplyr::filter(zackenberg, Habitat == "Art4")
art4$Trap_number <- (paste(art4$Trap_number, "4", sep=""))
mes <- rbind(art3, art4)
mes$Habitat <- "Mesic"
zackenberg <- rbind(hum, mes)

#........ajouter nb_of_traps ----
dfZA<-data.frame()

# boucle sur les années 
years<- unique(zackenberg$Period_Year)
for (i in 1: length(years)) {
  
  y <- years[i]
  tab2<- zackenberg[which(zackenberg$Period_Year == y), ]
  
  #boucle sur l'habitat
  habitat <-unique(tab2$Area_Name)
  
  for (k in 1:length(habitat)){
    
    hab <- habitat[k]
    tab3 <-  tab2[which(tab2$Area_Name == hab), ]
    
    #créer banque de jours
    days <- sort(unique(as.numeric(as.character(tab3$Period_Day))))
    
    
    #boucle sur les jours
    for (j in 1:length(days)) {
      
      day <- days[j]
      tab4 <- tab3[which(tab3$Period_Day == day), ]
      
      # Nombre de trappes pour ce jour/annee/site (tout habitat confondu)
      n_trap   <- length(unique(as.character(tab4$Trap_number)))
      
      tab4$nb_of_trap <- n_trap
      
      
      
      
      
      dfZA<-rbind(dfZA, tab4)
      
    }}}
source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
dfZA<-daily_biomass(dfZA)
write.table(dfZA, file = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/dfZA_pre_conv.txt")

source('~/Desktop/MAITRISE/scripts/CAL_conv_fun.R')
dfZA<-CAL_conv(dfZA)

return(dfZA)
}

