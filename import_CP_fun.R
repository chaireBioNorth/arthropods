#libraries
library(dplyr)
library(lubridate)
#import data ----

chipp2013mesic <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/2013 ASDN Chipp River mesic tundra analysis.txt", sep = "\t", header = T, dec = ",")

import_CP<- function(data){ 
chipp2013mesic <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/2013 ASDN Chipp River mesic tundra analysis.txt", sep = "\t", header = T, dec = ",")
chipp2013dry <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/2013 ASDN Chipp River dry tundra analysis.txt", sep = "\t", header = T, dec = ",")
chipp <- dplyr::full_join(chipp2013dry, chipp2013mesic)

# gestion date ----
chipp$Date <- as.character(chipp$Date)
chipp$Date <- as.Date(chipp$Date, format= "%Y-%m-%d" )
chipp$Period_Year <- year(chipp$Date)
ref2 <- paste(chipp$Period_Year, "-01-01", sep = "")
ref2 <- as.Date(ref2)

chipp$Period_Day <- (chipp$Date - ref2)+1
chipp$Period_Day <- as.integer(chipp$Period_Day)

# homogénéiser noms de colonnes
names(chipp) <- c("Field_site", "Habitat", "Date", "Trap_number", "Arthropod_Taxon", "Stage", "Insect", "Origin", "OtherID", "Arthropod_Order", "Arthropod_Family", "Common_name", "Arthropod_Count", "Arthropod_Biomass", "Period_Year", "Period_Day")
chipp$Trap_number <- as.factor(chipp$Trap_number)
chipp$Field_site <- "Chipp"
return(chipp)
}

