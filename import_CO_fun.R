#libraries
library(dplyr)
library(lubridate)
#import data ----
colville2015dry <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2015 ASDN Colville dry tundra analysis.txt", sep = "\t", header = T, dec = ",")
import_CO<- function(data){ 
colville2015dry <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2015 ASDN Colville dry tundra analysis.txt", sep = "\t", header = T, dec = ",")
colville2015mesic <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2015 ASDN Colville mesic tundra analysis.txt", sep = "\t", header = T, dec = ",")
colville2016mesic <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2016 ASDN Colville mesic tundra analysis.txt", sep = "\t", header = T, dec = ",")
colville2016dry <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2016 ASDN Colville dry tundra analysis.txt", sep = "\t", header = T, dec = ",")
colville2017mesic <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2017 ASDN Colville mesic tundra analysis.txt", sep = "\t", header = T, dec = ",")
colville2017dry <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_colville/2017 ASDN Colville dry tundra analysis.txt", sep = "\t", header = T, dec = ",")
colville <- dplyr::full_join(colville2015dry, colville2015mesic)
colville <- dplyr::full_join(colville, colville2016dry)
colville <- dplyr::full_join(colville, colville2016mesic)
colville <- dplyr::full_join(colville, colville2017dry)
colville <- dplyr::full_join(colville, colville2017mesic)

# gestion date ----
colville$Date <- as.character(colville$Date)
colville$Date <- as.Date(colville$Date, format= "%Y-%m-%d" )
colville$Period_Year <- year(colville$Date)
ref2 <- paste(colville$Period_Year, "-01-01", sep = "")
ref2 <- as.Date(ref2)

colville$Period_Day <- (colville$Date - ref2)+1
colville$Period_Day <- as.integer(colville$Period_Day)

# homogénéiser noms de colonnes
names(colville) <- c("Field_site", "Habitat", "Date", "Trap_number", "Arthropod_Taxon", "Stage", "Insect", "Origin", "OtherID", "Arthropod_Order", "Arthropod_Family", "Common_name", "Arthropod_Count", "Arthropod_Biomass", "Arthropod_Length", "Period_Year", "Period_Day")
colville$Trap_number <- as.factor(colville$Trap_number)
return(colville)
}

