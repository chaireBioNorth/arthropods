
asdn_data <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/ASDN_Invert_biomass.txt", sep = "\t", header = T, dec = ".")

import_ASDN<- function(data){ 
  asdn_data <- data
  library(lubridate)
str(asdn_data)
# asdn_data <- dplyr::filter(asdn_data, Collection_Method != "bottle trap"&Collection_Method != "sweep net" )
asdn_data <- dplyr::filter(asdn_data, Collection_Method == "malaise trap")
asdn_data <- dplyr::filter(asdn_data, Origin != "Aquatic")
#gestion date ----
asdn_data$Date <- as.character(asdn_data$Date)
asdn_data$Date <- as.Date(asdn_data$Date, format= "%y-%m-%d" )
asdn_data$Period_Year <- year(asdn_data$Date)
ref2 <- paste(asdn_data$Period_Year, "-01-01", sep = "")
ref2 <- as.Date(ref2)

asdn_data$Period_Day <- (asdn_data$Date - ref2)+1
asdn_data$Period_Day <- as.integer(asdn_data$Period_Day)

#range périodes échantillonnage----
#exploration période à couvrir
data<-asdn_data
# range_jours<- data.frame()
# sites <- unique(data$Field_site)
# for (i in 1:length(sites)) {
#   site <- sites[i]
#   tab1 <- data[which(data$Field_site == site),]
#   
#   years <- unique(tab1$Period_Year)  
#   
#   for (j in 1:length(years)) {
#     year <- years[j]
#     
#     tab2 <- tab1[which(tab1$Period_Year == year),]
#     range <- range(tab2$Period_Day)  
#     long <-  range[2] - range[1]
#     
#     tab3 <- data.frame(Field_site = unique(tab2$Field_site), Period_Year = unique(tab2$Period_Year), deb = range[1], fin= range[2],  long = long)  
#     
#     range_jours<- rbind(tab3, range_jours)
#   }
# }

# Un peu d'uniformisation (noms de colonnes) ----
str(asdn_data)
names(asdn_data) <- c("Field_site", "Year", "Habitat", "Trap_type", "Date", "Trap_number", "Arthropod_Taxon", "Stage", "Insect", "Origin", "OtherID", "Arthropod_Order", "Arthropod_Family", "Common_name", "Arthropod_Count", "Arthropod_Biomass", "Period_Year", "Period_Day" )
#
# ajout Colville récent ----
colville2015dry <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2015 ASDN Colville dry tundra analysis.txt", sep = "\t", header = T, dec = ",")
source('~/Desktop/MAITRISE/scripts/import_CO_fun.R')

dfCO <- import_CO(data=colville2015dry)
asdn_data<-dplyr::full_join(asdn_data, dfCO)

# ajout Chipp récent ----
chipp2013mesic <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/2013 ASDN Chipp River mesic tundra analysis.txt", sep = "\t", header = T, dec = ",")
source('~/Desktop/MAITRISE/scripts/import_CP_fun.R')

dfCP <- import_CP(data=chipp2013mesic)
asdn_data<-dplyr::full_join(asdn_data, dfCP)

#élimination des saisons évidemment trop courtes ----
str(asdn_data)
asdn_data2 <- asdn_data[-which(asdn_data$Field_site == "cakr" & asdn_data$Period_Year == 2013),]
asdn_data3 <- asdn_data2[-which(asdn_data2$Field_site == "prba" & asdn_data2$Period_Year == "2011"),]
asdn_data <- asdn_data3
# ..........ajout des trap_days + nb_of_trap ----
df <- data.frame()
#boucle sur le site
sites <- unique(asdn_data$Field_site)
# boucle sur les années 

for(h in 1: length(sites)) {
  s <- sites[h]
  tab1<- asdn_data[which(asdn_data$Field_site == s), ]
  
  years<- unique(tab1$Period_Year)
for (i in 1: length(years)) {
  
  y <- years[i]
  tab2<- tab1[which(tab1$Period_Year == y), ]


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
      tab4$trap_days <- days[j]-days[j-1]
      tab4$nb_of_trap <- length(unique(tab4$Trap_number))
      
      df<-rbind(df, tab4)
    }}}}
df$Field_site <- as.character(df$Field_site)
df$Field_site[df$Field_site == "colv"] <- "Colville"
df$Field_site[df$Field_site == "barr"] <- "Utqiagvik"
df$Field_site[df$Field_site == "cakr"] <- "Krusenstern"
df$Field_site[df$Field_site == "eaba"] <- "Southampton"
df$Field_site[df$Field_site == "made"] <- "Mackenzie"
df$Field_site[df$Field_site == "cari"] <- "Canning"
df$Field_site[df$Field_site == "ikpi"] <- "Ikpikpuk"
df$Field_site[df$Field_site == "nome"] <- "Nome"
df$Field_site[df$Field_site == "prba"] <- "Prudhoe"

df <- df[,-c(2,5,9,10,14)]
source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
df2<-daily_biomass(df)
return(df2)
}
