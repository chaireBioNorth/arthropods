# simplification du script lissage_moyenne_mobile2.R -> juste habitats ensemble et sans les sections éliminées
#importation des données
library(dplyr)
# [[[[ MOYENNE DES HABITATS ]]]] ----
data <- read.table("DATA/FINAL_DATA/mg_trappe_jour_moy_des_habs.txt")
extract_param <- function(data){

data <- dplyr::filter(data, !is.na(mg_jour_trappe))

AK <-subset(data, Field_site == "Akimiski")
AL <- subset(data, Field_site == "Alert")
BY <- subset(data, Field_site == "Bylot")
CH <- subset(data, Field_site == "Churchill")
IG <- subset(data, Field_site == "Igloolik")
HO <- subset(data, Field_site == "Hochstetter")
HS <- subset(data, Field_site == "Herschel")
SO <- subset(data, Field_site == "Southampton")
TA <- subset(data, Field_site == "Taimyr")
ZA <- subset(data, Field_site == "Zackenberg")
HS <- subset(HS, Period_Year == "2007" | Period_Year == "2008")
UT <- subset(data, Field_site == "Utqiagvik")
NO <- subset(data, Field_site == "Nome")
PR <- subset(data, Field_site == "Prudhoe")
CA <- subset(data, Field_site == "Canning")
IK <- subset(data, Field_site == "Ikpikpuk")
MA <- subset(data, Field_site == "Mackenzie") 
KR <- subset(data, Field_site == "Krusenstern")
CO <- subset(data, Field_site == "Colville")
CP <- subset(data, Field_site == "Chipp")


jeux<- list(AK,AL, UT, BY, CA, CH,CO, CP, HO, HS,IG,IK, KR, MA, NO, PR, SO, TA, ZA)



# [ DONNÉES BRUTES ] ----
# extraction de la date et la hauteur du pic ----

rawdata <- data.frame()

for (i in 1:length(jeux)) {
  
  tab1 <- jeux[[i]]
  
  #boucle sur les années
  years <- unique(tab1$Period_Year)
  for (j in 1:length(years)) {
    
    year <- years[j]
    tab2 <- tab1[which(tab1$Period_Year== year), ]  
    
    peak_ampli<- max(tab2$mg_jour_trappe, na.rm = T)
    
   tab3 <- tab2[which(tab2$mg_jour_trappe == peak_ampli),]
  
      peak_date <- round(median(tab3$Period_Day), digits=0)
        
        
 # max(tab2$Period_Day[which(tab2$mg_jour_trappe == peak_ampli)])
 # peak_date <-round(median(peak_date_test), digits = 0)
      # 
    
 
    
    tab4 <- data.frame(Field_site = unique(tab3$Field_site), Period_Year = unique(tab3$Period_Year), Peak_amplitude_raw = peak_ampli, Peak_date_raw = peak_date)
    
    rawdata <- rbind(rawdata, tab4)
  }}


#calcul de la biomasse saisonnière avec période relative au pic----
#exploration période à couvrir

range_jours<- data.frame()
sites <- unique(data$Field_site)
for (i in 1:length(sites)) {
  site <- sites[i]
  tab1 <- data[which(data$Field_site == site),]
  
  years <- unique(tab1$Period_Year)  
  
  for (j in 1:length(years)) {
    year <- years[j]
    
    tab2 <- tab1[which(tab1$Period_Year == year),]
    range <- range(tab2$Period_Day)  
    range1<- range[1]
    tab_deb<- tab2[which(tab2$Period_Day==range1),]
    deb<-range1-tab_deb$trap_days
    long <- range[2]-deb
    
    
    
    tab3 <- data.frame (Field_site = unique(tab2$Field_site), Period_Year = unique(tab2$Period_Year), deb_sais=deb, deb = range[1], fin= range[2], long=long)  
    
    range_jours<- rbind(tab3, range_jours)
  }
}
tab_apercu <- left_join(range_jours, rawdata, by=c("Field_site", "Period_Year"))

tab_apercu2 <- data.frame()


sites <- unique(tab_apercu$Field_site)
for (i in 1:length(sites)) {
  site <- sites[i]
  tab1 <- tab_apercu[which(tab_apercu$Field_site == site),]
  
  years <- unique(tab1$Period_Year)  
  
  for (j in 1:length(years)) {
    year <- years[j]
    
    tab2 <- tab1[which(tab1$Period_Year == year),]
    tab2$deb_pic <- tab2$Peak_date_raw - 10
    tab2$fin_pic <- tab2$Peak_date_raw + 10
    tab2$diff_deb <- tab2$deb_pic - tab2$deb
    tab2$diff_fin <- tab2$fin - tab2$fin_pic
    
    tab_apercu2 <- rbind(tab_apercu2, tab2)
  }}
# library(ggplot2)
# ggplot2::ggplot(rawdata, aes(x=Field_site, y=Peak_amplitude_raw))+
#   geom_point()
# ggplot2::ggplot(rawdata, aes(x=Field_site, y=Peak_date_raw))+
#   geom_point()

prob_deb <- dplyr::filter(tab_apercu2, deb_pic < deb)

prob_fin <- dplyr::filter(tab_apercu2, fin_pic > fin)


data_limites <- left_join(data, tab_apercu2, by=c("Field_site", "Period_Year"))
data_limites <- dplyr::filter(data_limites, !is.na(Peak_amplitude_raw))


subs_bio <- data.frame()
subs_prob <-data.frame()
sites = unique(data_limites$Field_site)
for (a in 1:length(sites)){
  site = sites[a]
  tab1 <- data_limites[which(data_limites$Field_site == site),]
  
  years = unique(tab1$Period_Year)
  for (b in 1:length(years)){
    year = years[b]
    tab2 <- tab1[which(tab1$Period_Year == year),]
    d <- unique(tab2$diff_deb)
    f <- unique(tab2$diff_fin)
    if (f >= 0 & d >= 0) { 
      
      aire <- dplyr::filter(tab2, Period_Day >= deb_pic & Period_Day <= fin_pic )
      subs_bio <- rbind(subs_bio, aire)
    } else { 
      assign(paste(site, year, sep = "_"),tab2)
      
    }
  }}

#prob fin
subs_bylot2005 <- dplyr::filter(Bylot_2005, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)
subs_utqiagvik2011 <- dplyr::filter(Utqiagvik_2011, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
subs_mackenzie2011 <- dplyr::filter(Mackenzie_2011, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
subs_mackenzie2010 <- dplyr::filter(Mackenzie_2010, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
subs_southampton2007 <- dplyr::filter(Southampton_2007, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
subs_utqiagvik2010 <- dplyr::filter(Utqiagvik_2010, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
subs_hochstetter2012 <- dplyr::filter(Hochstetter_2012, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )

# subs_zack1998 <- dplyr::filter(Zackenberg_1998,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )

# subs_colville2011 <- dplyr::filter(Colville_2011,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
# subs_zack2010 <- dplyr::filter(Zackenberg_2010,Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
# subs_canning2012 <- dplyr::filter(Canning_2012,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )


#prob début
subs_akimiski2009 <- dplyr::filter(Akimiski_2009, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
subs_canning2011 <- dplyr::filter(Canning_2011, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )

subs_bio <- rbind(subs_bio, subs_bylot2005, subs_utqiagvik2011,subs_mackenzie2011, subs_mackenzie2010,subs_southampton2007, subs_utqiagvik2010, subs_canning2011,subs_akimiski2009, subs_hochstetter2012)



#en considérant des rectangles par bloc de trappe ie area under curve
auc_raw <- data.frame()

sites <- unique(subs_bio$Field_site)
for (i in 1:length(sites)) {
  site <- sites[i]
  tab1 <- subs_bio[which(subs_bio$Field_site == site),]
  
  years <- unique(tab1$Period_Year)  
  
  for (j in 1:length(years)) {
    year <- years[j]
    
    tab2 <- tab1[which(tab1$Period_Year == year),]
    tab2$masse_acc <- tab2$mg_jour_trappe
    somme <- sum(tab2$mg_jour_trappe)
    tab3 <- data.frame (Field_site = unique(tab2$Field_site), Period_Year = unique(tab2$Period_Year), somme_sais_raw = somme)  
    
    auc_raw<- rbind( auc_raw, tab3)
  }
}

param <- left_join(rawdata, auc_raw, by = c("Field_site", "Period_Year"))

#  [ MOYENNE MOBILE ]  ----

# Smoothed symmetrically:
# 
# data_mm <- data.frame()
# MMdata<- data.frame()
# for (i in 1:length(jeux)) {
#   
#   dat <- jeux[[i]]
#   
#   #boucle sur les années
#   years <- unique(jeux[[i]]$Period_Year)
#   for (j in 1:length(years)) {
#     
#     year <- years[j]
#     
#     datas <- dat[which(dat$Period_Year == year),]
#     
#     fen   <- rep(1/5, 5)            #ici on définit la largeur de la fenêtre
#     y_sym <- stats::filter(datas$mg_jour_trappe, fen, sides=2)  
#     
#     pos <- which(!is.na(y_sym))         # pos = tout ce qui n'est pas NA
#     y_sym = y_sym[pos]
#     x_sym =  datas$Period_Day[pos]
#     
#     
#     #identifier les peaks
#     peaks <- cardidates::peakwindow(x = x_sym, y = y_sym, minpeak = 0.1, mincut = 0.6) # 
#     
#     peaks<- peaks$peaks  # juste pour sortir de la liste
#     peakvalue <- max(peaks$y)  # extraire valeur max
#     peakdate  <- peaks$x[which(peaks$y == peakvalue)]  # extraire date de la valeur max
#     
#     # valeurs prédites
#     result <- data.frame( x_sym, y_sym)
#     result$Field_site <- unique(datas$Field_site)      
#     result$Period_Year <- unique(datas$Period_Year)
#     data_mm <- rbind(data_mm, result)
#     # Extraction de la date et hauteur du pic  ----
#     #paramètres extraits
#     peak <- data.frame(Field_site = unique(datas$Field_site), Period_Year = unique(datas$Period_Year), Peak_amplitude_MM =peakvalue, Peak_date_MM =peakdate)
#     MMdata <-rbind(MMdata, peak)
#     
#   }
# }


#calcul de la biomasse saisonnière avec période relative au pic----
#exploration période à couvrir
# 
# 
# tab_apercu <- left_join(range_jours, MMdata, by=c("Field_site", "Period_Year"))
# tab_apercu2 <- data.frame()
# 
# 
# sites <- unique(tab_apercu$Field_site)
# for (i in 1:length(sites)) {
#   site <- sites[i]
#   tab1 <- tab_apercu[which(tab_apercu$Field_site == site),]
#   
#   years <- unique(tab1$Period_Year)  
#   
#   for (j in 1:length(years)) {
#     year <- years[j]
#     
#     tab2 <- tab1[which(tab1$Period_Year == year),]
#     tab2$deb_pic <- tab2$Peak_date_MM - 10
#     tab2$fin_pic <- tab2$Peak_date_MM + 10
#     tab2$diff_deb <- tab2$deb_pic - tab2$deb
#     tab2$diff_fin <- tab2$fin - tab2$fin_pic
#     
#     tab_apercu2 <- rbind(tab_apercu2, tab2)
#   }}
# 
# 
# prob_debMM <- dplyr::filter(tab_apercu2, deb_pic < deb)
# 
# prob_finMM <- dplyr::filter(tab_apercu2, fin_pic > fin)
# 
# 
# data_limites <- left_join(data, tab_apercu2, by=c("Field_site", "Period_Year"))
# data_limites <- dplyr::filter(data_limites, !is.na(Peak_amplitude_MM))
# 
# 
# subs_bio <- data.frame()
# subs_prob <-data.frame()
# sites = unique(data_limites$Field_site)
# for (a in 1:length(sites)){
#   site = sites[a]
#   tab1 <- data_limites[which(data_limites$Field_site == site),]
#   
#   years = unique(tab1$Period_Year)
#   for (b in 1:length(years)){
#     year = years[b]
#     tab2 <- tab1[which(tab1$Period_Year == year),]
#     d <- unique(tab2$diff_deb)
#     f <- unique(tab2$diff_fin)
#     if (f >= 0 & d >= 0) { 
#       
#       aire <- dplyr::filter(tab2, Period_Day >= deb_pic & Period_Day <= fin_pic )
#       subs_bio <- rbind(subs_bio, aire)
#     } else { 
#       assign(paste(site, year, sep = "_"),tab2)
#       
#     }
#   }}
# 
# # prob fin
# subs_utqiagvik2011 <- dplyr::filter(Utqiagvik_2011, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
# subs_utqiagvik2010 <- dplyr::filter(Utqiagvik_2010, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
# subs_bylot2005 <- dplyr::filter(Bylot_2005, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)
# subs_zack1998 <- dplyr::filter(Zackenberg_1998,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
# subs_mackenzie2010 <- dplyr::filter(Mackenzie_2010,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
# subs_mackenzie2011 <- dplyr::filter(Mackenzie_2011,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
# 
# 
# #prob début
# subs_akimiski2009 <- dplyr::filter(Akimiski_2009, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
# subs_canning2011 <- dplyr::filter(Canning_2011, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
# 
# subs_bio <- rbind(subs_bio, subs_bylot2005, subs_zack1998,subs_canning2011,subs_akimiski2009, subs_utqiagvik2011, subs_utqiagvik2010, subs_mackenzie2010, subs_mackenzie2011)
# 
# 
# 
# #en considérant des rectangles par bloc de trappe ie area under curve
# auc_MM <- data.frame()
# 
# sites <- unique(subs_bio$Field_site)
# for (i in 1:length(sites)) {
#   site <- sites[i]
#   tab1 <- subs_bio[which(subs_bio$Field_site == site),]
#   
#   years <- unique(tab1$Period_Year)  
#   
#   for (j in 1:length(years)) {
#     year <- years[j]
#     
#     tab2 <- tab1[which(tab1$Period_Year == year),]
#     tab2$masse_acc <- tab2$mg_jour_trappe
#     somme <- sum(tab2$mg_jour_trappe)
#     tab3 <- data.frame (Field_site = unique(tab2$Field_site), Period_Year = unique(tab2$Period_Year), somme_sais_MM = somme)  
#     
#     auc_MM<- rbind( auc_MM, tab3)
#   }
# }
# 
# paramMM <- left_join(MMdata, auc_MM, by = c("Field_site", "Period_Year"))

# paramMM <- MMdata








# fusions finales ----
# param <- left_join(param, paramMM, by= c("Field_site", "Period_Year"))

param <- dplyr::filter(param, Period_Year!= "2018") # pas de données météo
param$zone <- NA
param$zone[param$Field_site=="Akimiski"]<-"10"
param$zone[param$Field_site=="Alert"]<-"1"
param$zone[param$Field_site=="Bylot"]<-"3"
param$zone[param$Field_site=="Canning"]<-"4"
param$zone[param$Field_site=="Colville"]<-"4"
param$zone[param$Field_site=="Krusenstern"]<-"5"
param$zone[param$Field_site=="Churchill"]<-"5"
param$zone[param$Field_site=="Herschel"]<-"4"
param$zone[param$Field_site=="Igloolik"]<-"3"
param$zone[param$Field_site=="Ikpikpuk"]<-"4"
param$zone[param$Field_site=="Mackenzie"]<-"4"
param$zone[param$Field_site=="Nome"]<-"5"
param$zone[param$Field_site=="Prudhoe"]<-"4"
param$zone[param$Field_site=="Southampton"]<-"3"
param$zone[param$Field_site=="Taimyr"]<-"3"
param$zone[param$Field_site=="Utqiagvik"]<-"3"
param$zone[param$Field_site=="Zackenberg"]<-"2"
param$zone[param$Field_site=="Hochstetter"]<-"2"

return(param)



}



