# .....[ Igloolik ] ----
import_IG <- function(data){ 
igloolik <-data

#déjà presque pareil à arthro_all

#enlever lignes vides
igloolik <- dplyr::filter(igloolik, !is.na(ID))


#gestion date (JJ-MM-AA -> jour julien)

igloolik$Period_Day <- as.character(igloolik$Period_Day)

#14-07-01
igloolik$Period_Day <- as.Date(igloolik$Period_Day, format= "%y-%m-%d" )
ref2 <- paste(igloolik$Period_Year, "-01-01", sep = "")
ref2 <- as.Date(ref2)

igloolik$Period_Day <- (igloolik$Period_Day - ref2)+1
igloolik$Period_Day <-as.integer(igloolik$Period_Day)

#conversion g --> mg
igloolik$Arthropod_Biomass <- igloolik$Arthropod_Biomass*1000


#pour homogénéiser noms d'habitats et avoir 2 niveaux de facteur
igl_mes <- igloolik[-which(igloolik$Habitat == "humid"),]
igl_mes$Habitat <- "mesic"
igl_mes$Habitat <- as.factor(igl_mes$Habitat)
igl_hum <- igloolik[which(igloolik$Habitat == "humid"),]
igl_hum$Habitat <- "humid"
igl_hum$Habitat <- as.factor(igl_hum$Habitat)

igloolik <- rbind(igl_hum, igl_mes)

# ..........ajout des trap_days ----
# boucle sur les années 
dfIG <- data.frame()
years<- unique(igloolik$Period_Year)
for (i in 1: length(years)) {
  
  y <- years[i]
  tab2<- igloolik[which(igloolik$Period_Year == y), ]
  
  #boucle sur l'habitat
  habitat <-unique(tab2$Habitat)
  
  for (k in 1:length(habitat)){
    
    hab <- habitat[k]
    tab3 <-  tab2[which(tab2$Habitat == hab), ]
    
    #boucle sur les jours 
    #créer banque de jours
    days <- sort(unique(as.numeric(as.character(tab3$Period_Day))))
    length(days)
    days 
    day <-days[1]
    tab4 <- tab3[which(tab3$Period_Day == day), ]
    tab4$trap_days <- 2
    dfIG<-rbind(dfIG, tab4)
    
    
    for (j in 2:length(days)) {
      
      day <- days[j]
      tab4 <- tab3[which(tab3$Period_Day == day), ]
      tab4$trap_days <- days[j]-days[j-1]
      
      
      dfIG<-rbind(dfIG, tab4)
    }}}
dfIG$Arthropod_Family <- as.character(dfIG$Arthropod_Family)
dfIG$Trap_number <- as.character(dfIG$Trap_number)
dfIG$OtherID <- as.character(dfIG$OtherID)
dfIG$Stage <- as.character(dfIG$Stage)
dfIG$Area_Name <- as.character(dfIG$Area_Name)
dfIG$Sample_Location <- as.character(dfIG$Sample_Location)

source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
dfIG<-daily_biomass(dfIG)
return(dfIG)
}
