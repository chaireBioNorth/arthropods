library(tidyr)
library(dplyr)
library(lubridate)
library(WriteXLS)




setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/raw/")
akimiski_raw <- read.csv2(file= "Akimiski_CCIN1736_20120124_ArcticWOLVES_arthropod_monitoring.csv", dec=".")
arthro_all_tout <- read.csv(file = "arthro_all_TOUT.csv")
CH <- dplyr::filter(arthro_all_tout, Field_site== "Churchill")
str(akimiski_raw)
akimiski_raw$Arthropod_Biomass[akimiski_raw$Arthropod_Biomass == -99999] <- NA
akimiski_raw$Arthropod_Length[akimiski_raw$Arthropod_Length == -99999] <- NA
ak <- akimiski_raw
#gestion date ----
ak$Period_Date <- as.character(ak$Period_Date)
ak$Period_Date <- as.Date(ak$Period_Date, format= "%d/%m/%Y" )
ak$Period_Year <- year(ak$Period_Date)
ref2 <- paste(ak$Period_Year, "-01-01", sep = "")
ref2 <- as.Date(ref2)

ak$Period_Day <- (ak$Period_Date - ref2)+1
ak$Period_Day <- as.integer(ak$Period_Day)


# explo données ----
ak2008 <- dplyr:: filter(ak, Period_Year == "2008")
ak2009 <- dplyr:: filter(ak, Period_Year == "2009")

# #liste taxons
# ak2009$Taxon <- paste(ak2009$Arthropod_Order, ak2009$Arthropod_Family, sep="." )
# taxons <- as.data.frame(unique(ak2009$Taxon))
# 
# tax09 <- separate(data = taxons, col =1, into = c("Arthropod_Order", "Arthropod_Family"), sep = "\\.")
# 
# WriteXLS(tax09, "taxons_Akimiski2009.xls")
# 
# ak2008$Taxon <- paste(ak2008$Arthropod_Order, ak2008$Arthropod_Family, sep="." )
# taxons <- as.data.frame(unique(ak2008$Taxon))
# 
# tax08 <- separate(data = taxons, col =1, into = c("Arthropod_Order", "Arthropod_Family"), sep = "\\.")
# 
# WriteXLS(tax08, "taxons_Akimiski2008.xls")


#pour connaître l'occurence d'un taxon ----
length(which(ak2008$Arthropod_Family == "Tettiridae"))
(which(ak2009$Arthropod_Family == "Tenebrionidae"))
ak2009[2704,]



length(which(ak2009$Arthropod_Family == "Tettiridae"))
tab <- ak2009[(which(ak2009$Arthropod_Family == "fam N/A" & ak2009$Arthropod_Order == "Other")),]     
plot(tab$Period_Day, tab$Arthropod_Count)
hist(ak2008$Arthropod_Biomass/ak2008$Arthropod_Count)

#extraction moyennes et médianes ----

taxons2009 <- read.csv("tax_AK0809.csv", sep=";")
fam <- taxons2009$Family
fam2 <- taxons2009$Arthropod_Family
big_dat<-data.frame()
probs <- data.frame()
for (i in 1:length(fam2)){
  gr <- dplyr::filter(ak2008, Arthropod_Family == as.character(fam[i]) ) 
if (length (gr[,1]) >= 4 ) {
  
  moy_bio <- mean(gr$Arthropod_Biomass/gr$Arthropod_Count, na.rm=T)
  med_bio <-median(gr$Arthropod_Biomass/gr$Arthropod_Count, na.rm=T)
  sd_bio <- sd(gr$Arthropod_Biomass/gr$Arthropod_Count, na.rm=T)
  
  
  moy_long <- mean(gr$Arthropod_Length, na.rm=T)
  med_long <-median(gr$Arthropod_Length, na.rm=T)
  sd_long <- sd(gr$Arthropod_Length, na.rm=T)
  
  dat<- data.frame(Family=fam[i], moy_bio=moy_bio,  sd_bio=sd_bio, med_bio=med_bio,moy_long=moy_long, sd_long=sd_long, med_long=med_long, Arthropod_Family = fam2[i])
  big_dat <- rbind(big_dat, dat)
} else {
  dat2 <- data.frame(Family = fam[i], n = length(gr[,1]), Arthropod_Family=fam2[i])
  probs <- rbind(probs, dat2)
}
}  
big_datCH <- data.frame() 
probsCH <- data.frame()
fam <- as.character(probs$Family)
fam2 <-as.character(probs$Arthropod_Family)
for (i in 1:length(fam2)){
  gr <- dplyr::filter(CH, Arthropod_Family == as.character(fam[i]) ) 
  if (length (gr[,1]) >=2  ) {
    
    moy_bio <- mean(gr$Arthropod_Biomass/gr$Arthropod_Count, na.rm=T)
    med_bio <-median(gr$Arthropod_Biomass/gr$Arthropod_Count, na.rm=T)
    sd_bio <- sd(gr$Arthropod_Biomass/gr$Arthropod_Count, na.rm=T)
    
    
    moy_long <- mean(gr$Arthropod_Length, na.rm=T)
    med_long <-median(gr$Arthropod_Length, na.rm=T)
    sd_long <- sd(gr$Arthropod_Length, na.rm=T)
    
    dat<- data.frame(Family=fam[i], moy_bio=moy_bio,  sd_bio=sd_bio, med_bio=med_bio,moy_long=moy_long, sd_long=sd_long, med_long=med_long, Arthropod_Family = fam2[i])
    big_datCH <- rbind(big_datCH, dat)
  } else {
    dat2 <- data.frame(Family = fam[i], n = length(gr[,1]) )
    probsCH <- rbind(probsCH, dat2)
  }
}

big_dat$source <- "Akimiski2008"
big_datCH$source <- "Churchill_allY"
ref_ak <- rbind(big_dat, big_datCH) 

#exceptions ----
tricho <- dplyr::filter(ak2008, Arthropod_Order == "Trichoptera")
mean(tricho$Arthropod_Biomass/tricho$Arthropod_Count)
sd(tricho$Arthropod_Biomass/tricho$Arthropod_Count)
median(tricho$Arthropod_Biomass/tricho$Arthropod_Count)
moy_bio <- weighted.mean(x=tricho$Arthropod_Biomass/tricho$Arthropod_Count,w=tricho$Arthropod_Count, na.rm =T )
moy_long <- weighted.mean(x=tricho$Arthropod_Length, w = tricho$Arthropod_Count, na.rm=T)
dat<- data.frame(Family="Trichoptera NA", moy_bio=NA,  sd_bio=NA, med_bio=moy_bio,moy_long=moy_long, sd_long=NA, med_long=NA, Arthropod_Family = "Trichoptera fam N/A", source = "Akimiski2008")
ref_ak <- rbind(ref_ak, dat)
WriteXLS(ref_ak, ExcelFileName = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/Ref_ak.xls")
#others fam N/A == on élimine, on sait pas ce que c'est
# mean(ak2008$Arthropod_Biomass/ak2008$Arthropod_Count, na.rm=T)
# sd(ak2008$Arthropod_Biomass/ak2008$Arthropod_Count, na.rm=T)
# median(ak2008$Arthropod_Biomass/ak2008$Arthropod_Count, na.rm=T)
# moy_bio <- weighted.mean(x=ak2008$Arthropod_Biomass/ak2008$Arthropod_Count,w=ak2008$Arthropod_Count, na.rm =T )
# moy_long <- weighted.mean(x=ak2008$Arthropod_Length, w = ak2008$Arthropod_Count, na.rm=T)
# dat<- data.frame(Family="Other", moy_bio=NA,  sd_bio=NA, med_bio=moy_bio,moy_long=moy_long, sd_long=NA, med_long=NA, Arthropod_Family = "fam N/A",  source = "Akimiski2008")
# ref_ak <- rbind(ref_ak, dat)




akimiski <- left_join(ak2009, ref_ak, by = "Arthropod_Family")
akimiski <- dplyr::filter(akimiski, Arthropod_Order !="Other")
akimiski$Arthropod_Biomass <- akimiski$Arthropod_Count*akimiski$med_bio
prob <- dplyr::filter(akimiski, is.na(Arthropod_Biomass))
unique(prob$Arthropod_Family)
akimiski <- dplyr::filter(akimiski, !is.na(Arthropod_Biomass))
# akimiski$Habitat <- "humid"
akimiski$Field_site <- "Akimiski"
write.table(akimiski, file = "akimiski_std.txt")
ak <-read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/akimiski_std.txt")
