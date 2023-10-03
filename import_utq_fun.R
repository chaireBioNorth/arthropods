utqiagvik <- read.csv("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/Utqiagvik_invertebrate_biomass.csv")


import_utq_fun <- function(data){
 utqiagvik <- data 
  library(lubridate)
  ut_sampling <-read.csv("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/Utqiagvik_invertebrate_sampling_effort.csv")  
  names(ut_sampling) <- c("date", "nb_of_traps", "trap_days", "nb_cups", "nb_aqua", "comments2")
UT <- dplyr::left_join(utqiagvik, ut_sampling)
UT <- dplyr::filter(UT, collection_method == "malaise trap")

#gestion date ----
UT$date <- as.character(UT$date)
UT$date <- as.Date(UT$date, format= "%Y-%m-%d" )
UT$Period_Year <- year(UT$date)
ref2 <- paste(UT$Period_Year, "-01-01", sep = "")
ref2 <- as.Date(ref2)

UT$Period_Day <- (UT$date - ref2)+1
UT$Period_Day <- as.integer(UT$Period_Day)

#uniformisation et ménage
UT <- UT[,c(23,24,2,3, 6:10,13,16:19,22 )]
names(UT) <- c("Period_Year", "Period_Day", "Field_site", "Habitat", "Trap_number", "Arthropod_Family", "Stage", "Arthropod_Length", "Arthropod_Count", "Notes", "dry_mass", "Arthropod_Biomass","nb_of_traps_ALL", "trap_days", "comments_sampling")
UT <- dplyr::filter(UT, Period_Year =="2015"| Period_Year =="2016")
# ..........ajout des trap_days + nb_of_trap ----
df <- data.frame()
#boucle sur le site
sites <- unique(UT$Field_site)

  
  years<- unique(UT$Period_Year)
  for (i in 1: length(years)) {
    
    y <- years[i]
    tab2<- UT[which(UT$Period_Year == y), ]
    
    
    #boucle sur l'habitat
    habitat <-unique(tab2$Habitat)
    
    for (k in 1:length(habitat)){
      
      hab <- habitat[k]
      tab3 <-  tab2[which(tab2$Habitat == hab), ]
      
      #boucle sur les jours 
      #créer banque de jours
      days <- sort(unique(as.numeric(as.character(tab3$Period_Day))))
      
      for (j in 2:length(days)) {
        
        day <- days[j]
        tab4 <- tab3[which(tab3$Period_Day == day), ]
        tab4$trap_days2 <- days[j]-days[j-1]
        tab4$nb_of_trap <- length(unique(tab4$Trap_number))
        
        df<-rbind(df, tab4)
        
      }}}
  df2 <- df
  df2$Field_site <-as.character(df2$Field_site)
  df2$Field_site[df2$Field_site == "barr"] <- "Utqiagvik"
  #correction numéro de trappe (Voir commentaire)
#   correction <- dplyr::filter(df2, Period_Year == "2014" & Period_Day =="211")
#   df2 <- df2[-which(df2$Period_Year == "2014" & df2$Period_Day =="211"),]
#   gr1 <-dplyr::filter(correction, Trap_number == "D3" |Trap_number == "D4" |Trap_number == "M3" |Trap_number == "M4" )
# gr2 <- correction[-which(correction$Trap_number == "D3" |correction$Trap_number == "D4" |correction$Trap_number == "M4"|correction$Trap_number == "M3"),]
#   
# gr1$trap_days <-3
# gr2$trap_days <- 2  
# df2 <- rbind(df2, gr1, gr2)  
  
df3 <- df2[,-c(11,13,16)]
source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
df4<-daily_biomass(df3)
return(df4)

}
