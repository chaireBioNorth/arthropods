# .......2010-2011 ----
import_CH <- function(data){ 
  churchill2010_2011 <-data



#renommer les colonnes comme ARTHROPOD_ALL
names(churchill2010_2011)[names(churchill2010_2011) == 'year'] <- 'Period_Year'
names(churchill2010_2011)[names(churchill2010_2011) == 'X'] <- 'ID'
names(churchill2010_2011)[names(churchill2010_2011) == 'habitat'] <- 'Area_Name'
names(churchill2010_2011)[names(churchill2010_2011) == 'site'] <- 'Habitat'
names(churchill2010_2011)[names(churchill2010_2011) == 'trap'] <- 'Trap_number'
names(churchill2010_2011)[names(churchill2010_2011) == 'julian'] <- 'Period_Day'
names(churchill2010_2011)[names(churchill2010_2011) == 'order'] <- 'Arthropod_Order'
names(churchill2010_2011)[names(churchill2010_2011) == 'family'] <- 'Arthropod_Family'
names(churchill2010_2011)[names(churchill2010_2011) == 'stage'] <- 'Stage'
names(churchill2010_2011)[names(churchill2010_2011) == 'count'] <- 'Arthropod_Count'
names(churchill2010_2011)[names(churchill2010_2011) == 'length'] <- 'Arthropod_Length'
names(churchill2010_2011)[names(churchill2010_2011) == 'Biomass'] <- 'Arthropod_Biomass'
churchill2010_2011$Field_site <- "Churchill"



#retirer milieu aquatique

CH2010_2011 <- dplyr::filter(churchill2010_2011, Area_Name == "Terrestrial")

unique(CH2010_2011$Habitat)
hum <- dplyr::filter(CH2010_2011, Habitat == "mesic")
hum$Habitat = "Humid"
mes <- dplyr::filter(CH2010_2011, Habitat == "dry")
CH2010_2011 <- rbind(hum, mes)


# ..........ajout trap_days ----
dfCH<-data.frame()
CH <- CH2010_2011
# boucle sur les années 
years<- unique(CH$Period_Year)
for (i in 1: length(years)) {
  
  y <- years[i]
  tab2<- CH[which(CH$Period_Year == y), ]
  
  #boucle sur l'habitat
  habitat <-unique(tab2$Habitat)
  
  for (k in 1:length(habitat)){
    
    hab <- habitat[k]
    tab3 <-  tab2[which(tab2$Habitat == hab), ]
    
    #créer banque de jours
    days <- sort(unique(as.numeric(as.character(tab3$Period_Day))))
    
    
    #boucle sur les jours pour ajouter trap_days
    # on part à j-1 parce qu'on ne sait pas le nombre de jours de trappage pour j
    
    for (j in 2:length(days)) {
      
      day <- days[j]
      tab4 <- tab3[which(tab3$Period_Day == day), ]
      
      
      tab4$trap_days<- NA
      tab4$trap_days <- days[j]-days[j-1]
      
      
      
      
      
      
      dfCH<-rbind(dfCH, tab4)
    }}}

dfCH<- dfCH[,-c(13:17)]
dfCH$Trap_number <- as.character(dfCH$Trap_number)

source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
dfCH <- daily_biomass(dfCH)
return(dfCH)

}
