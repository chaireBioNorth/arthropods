library(dplyr)
setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/")
# meteo <- read.table("/media/eliane/BlackBox/MAITRISE/DATA/tables_intermediaires/table_climate_CH_SO_ZA_TA.txt", header =  T )
meteo <- read.table("DATA/tables_intermediaires/table_climate_CH_SO_ZA_TA.txt", header =  T )
# meteo <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/table_climate_CH_SO_ZA_TA.txt", header =  T )
data<-meteo
import_meteo <- function(data){
  
  #importer neige---
  neige <- read.table(file="DATA/tables_intermediaires/date_fonte.txt")
 neige[neige=="Taimyr"]<-"Medusa"
  # neige <- read.table(file="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/date_fonte.txt")
  # neige <- read.table(file="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/date_fonte.txt")
  
  
# séparer meteo ERA par SITE ----
meteoSO <- meteo [, c(1,2,6,10,14,18,22,26,30)]
meteoCH <- meteo [, c(1,3,7,11,15,19,23,27,31)]


#importer zack
meteoZA <- read.table("DATA/tables_intermediaires/table_climate_zack_bon.txt", header = T)
# meteoZA <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/table_climate_zack_bon.txt", header = T)


#meteoTA toutes les années
meteoTA <- read.table("DATA/tables_intermediaires/table_climate_taimyr.txt", header = T)


meteoAL_BY <-read.table("DATA/tables_intermediaires/table_climate_BY_AL.txt", header = T)
# séparer par SITE
meteoBY <- meteoAL_BY [, c(1,2,4,6,8,10,12,14,16)]
meteoAL<- meteoAL_BY [, c(1,3,5,7,9,11,13,15,17)]

meteoHS <-read.table("DATA/tables_intermediaires/table_climate_HS.txt", header = T)


meteoIG <-read.table("DATA/tables_intermediaires/table_climate_igloolik.txt", sep = "\t", header = T)
meteoAK <- read.table("DATA/tables_intermediaires/table_climate_akimiski.txt", header = T )
meteoASDN <- read.table("DATA/tables_intermediaires/table_climate_ASDN_west.txt", sep = "\t", header = T)
meteoNO<- meteoASDN [, c(1,2,10,18,26,34,42,50,58)]
meteoKR<- meteoASDN [, c(1,3,11,19,27,35,43,51,59)]
meteoCO<- meteoASDN [, c(1,4,12,20,28,36,44,52,60)]
meteoPR<- meteoASDN [, c(1,5,13,21,29,37,45,53,61)]
meteoCA<- meteoASDN [, c(1,6,14,22,30,38,46,54,62)]
meteoIK<- meteoASDN [, c(1,7,15,23,31,39,47,55,63)]
meteoUT<- meteoASDN [, c(1,8,16,24,32,40,48,56,64)]
meteoMA<- meteoASDN [, c(1,9,17,25,33,41,49,57,65)]
meteoCP <-meteoASDN[,c(1, 66:73)]

meteoHO <-read.table("DATA/tables_intermediaires/table_climate_hoch.txt", sep = "\t", header = T)
#fonction pour renommer colonnes ERA interim ----
rename <- function(site, data) {
  names(data)[names(data) == paste0("clt_", site)] <- 'clt'


  names(data)[names(data) == paste0("pr_", site)] <- 'pr'
  names(data)[names(data) == paste0("rsds_", site)] <- 'rsds'
  names(data)[names(data) == paste0("snd_", site)] <- 'snd'
  names(data)[names(data) == paste0("tas_", site)] <- 'tas'
  names(data)[names(data) == paste0 ("tasmax_", site)] <- 'tasmax'
  names(data)[names(data) == paste0("tasmin_", site)] <- 'tasmin'
  names(data)[names(data) == paste0 ("tsl1_", site)] <- 'tsl1'
  return(data)
}

# ...renommer les colonnes pour chaque site (appliquer fonction rename) ----
meteoAK$Field_site = "Akimiski"
meteoAK <- rename(site="akimiski", data=meteoAK)


meteoAL$Field_site = "Alert"
meteoAL <- rename(site="alert", data=meteoAL)

meteoBY$Field_site = "Bylot"
meteoBY <- rename(site="bylot", data=meteoBY)


meteoCH$Field_site = "Churchill"
meteoCH <- rename(site="churchill", data=meteoCH)


meteoHS$Field_site = "Herschel"
meteoHS <- rename(site="herschel", data=meteoHS)


meteoIG$Field_site = "Igloolik"
meteoIG <- rename(site="igloolik", data=meteoIG)


meteoSO$Field_site = "Southampton"
meteoSO <- rename(site="southampton", data=meteoSO)


meteoTA$Field_site = "Medusa"
meteoTA <- rename(site="taimyr", data=meteoTA)


meteoZA$Field_site = "Zackenberg"
meteoZA <- rename(site="zack", data=meteoZA)
meteoZA$tsl1 <- NA

meteoCA$Field_site = "Canning"
meteoCA <- rename(site="canning", data=meteoCA)

meteoCO$Field_site = "Colville"
meteoCO <- rename(site="colville", data=meteoCO)

meteoIK$Field_site = "Ikpikpuk"
meteoIK <- rename(site="ikpikpuk", data=meteoIK)

meteoKR$Field_site = "Krusenstern"
meteoKR <- rename(site="krusenstern", data=meteoKR)

meteoMA$Field_site = "Mackenzie"
meteoMA <- rename(site="mackenzie", data=meteoMA)

meteoNO$Field_site = "Nome"
meteoNO <- rename(site="nome", data=meteoNO)

meteoPR$Field_site = "Prudhoe"
meteoPR <- rename(site="prudhoe", data=meteoPR)

meteoUT$Field_site = "Utqiagvik"
meteoUT <- rename(site="utqiagvik", data=meteoUT)

meteoCP$Field_site = "Chipp"
meteoCP <- rename(site="Chipp", data=meteoCP)

meteoHO$Field_site = "Hochstetter"
meteoHO <- rename(site="Hochstetter", data=meteoHO)

# ...gestion date, degrés jours et précipitations accumulées ----
sites <- list(meteoAK,meteoAL, meteoBY, meteoCH, meteoHS, meteoIG, meteoSO, meteoTA, meteoZA, meteoCA, meteoCO, meteoIK, meteoKR, meteoMA, meteoNO,meteoPR, meteoUT, meteoCP, meteoHO)
meteo_all_test <- data.frame()
meteo_all <- data.frame()
datadj <- data.frame()
datapr <- data.frame()
for (h in 1: length(sites)) {
  
  data <- sites[[h]]
  
  #  gestion date
  data$date <- as.character(data$date)
  data$date <- as.Date(data$date, format= "%Y-%m-%d" )
  data$Period_Year <- format(data$date, "%Y")
  data$month <-format(data$date, "%m")
  data$month <-as.character(as.numeric(data$month))
  ref2 <- paste(data$Period_Year, "-01-01", sep = "")
  ref2 <- as.Date(ref2)
  
  data$julian <- (data$date - ref2)+1
  data$Period_Day <-as.integer(data$julian)
  
  ##créer une colonne de température positive
  data$tpos <- as.numeric(data$tas)
  data$tpos[data$tpos<=0]=0
  
  
  # #garder été
  # summer <- subset(data, month%in% c(5,6,7,8,9,10))
  
  ##garder juste à partir de juin
 summer <- dplyr::filter(data, Period_Day >= 156 & Period_Day <= 273)

  ##boucle sur les années pour créer degrés jours
  
  
  years<- unique(summer$Period_Year)
  
  for ( i in 1: length(years)) {
    
    y <- years[i]
    tab1 <- summer[which(summer$Period_Year == y), ]
    
    tab1$tdd2 <- cumsum(tab1$tpos)
    datadj<- rbind(tab1, datadj)
  }
  
  
  ##saison avec pluie
  summer2 <- summer
  
  ##boucle sur les années pour créer degrés jours
  
  
  years<- unique(summer2$Period_Year)
  
  for ( i in 1: length(years)) {
    
    y <- years[i]
    tab1 <- summer2[which(summer2$Period_Year == y), ]
    
    tab1$pr_acc<- cumsum(tab1$pr)
    datapr<- rbind(tab1, datapr)
  }
  meteo_all_test <- left_join(datadj, datapr)
  
}
meteo_all <- meteo_all_test
meteo_all$Period_Year <- as.integer(meteo_all$Period_Year)


#degrés jour à une date déterminée ####

degres_jours196<- data.frame()

sites <- unique(meteo_all$Field_site)

for (i in 1:length(sites)) {
  
  site = sites[i]
  tab1 = meteo_all[which(meteo_all$Field_site == site),]
  
  
  years <- unique(tab1$Period_Year)
  
  for (j in 1:length(years)) {
    
    year <- years[j]
    tab2 <- tab1[which(tab1$Period_Year== year), ]  
    j196 <- tab2[(tab2$Period_Day == 196),]
    dj196 <- j196$tdd2
    tab3 <- data.frame(Field_site = unique(tab2$Field_site), 
                       Period_Year = unique(tab2$Period_Year),   dj196= dj196)
    
    degres_jours196 <- rbind(degres_jours196 , tab3)
  }}


#précipitations accumulées à une date déterminée ####

prec_acc196<- data.frame()

sites <- unique(meteo_all$Field_site)

for (i in 1:length(sites)) {
  
  site = sites[i]
  tab1 = meteo_all[which(meteo_all$Field_site == site),]
  
  
  years <- unique(tab1$Period_Year)
  
  for (j in 1:length(years)) {
    
    year <- years[j]
    tab2 <- tab1[which(tab1$Period_Year== year), ]  
    j196 <- tab2[(tab2$Period_Day == 196),]
    pr196 <- j196$pr_acc
    tab3 <- data.frame(Field_site = unique(tab2$Field_site), 
                       Period_Year = unique(tab2$Period_Year),   pr196= pr196)
    
    prec_acc196 <- rbind(prec_acc196 , tab3)
  }}

#température moyenne estivale ####
sites <- unique(meteo_all$Field_site)
temp_moyenne_estivale<- data.frame()
for (i in 1:length(sites)) {
  
  site = sites[i]
  tab1 = meteo_all[which(meteo_all$Field_site == site),]
  
  years <- unique(tab1$Period_Year)
  
  for (j in 1:length(years)) {
    
    year <- years[j]
    tab2 <- tab1[which(tab1$Period_Year== year), ]  
    jun <- subset(tab2, tab2$month == 6 )
    jul <- subset(tab2, tab2$month == 7 )
    Moy_juin_juillet <- mean(c(jun$tas, jul$tas))
    # pr_juin <- mean(jun$pr)
    Moy_juin <- mean(jun$tas)
    Moy_juillet <- mean(jul$tas)
    
    juin1_15 <- dplyr::filter(tab2, Period_Day >= 152 & Period_Day<=166)
    juin1_15 <- mean(juin1_15$tas)
    
    juin16_30 <-  dplyr::filter(tab2, Period_Day >= 167 & Period_Day<=181)
    juin16_30<- mean(juin16_30$tas)
    
    juil1_15 <- dplyr::filter(tab2, Period_Day >= 182 & Period_Day<=196)
    juil1_15 <- mean(juil1_15$tas)
    
    juil16_31<- dplyr::filter(tab2, Period_Day >= 197 & Period_Day<=212)
    juil16_31 <- mean(juil16_31$tas)
    tab3 <- data.frame(Field_site = unique(tab2$Field_site), 
                       Period_Year = unique(tab2$Period_Year),   Moy_juin_juillet = Moy_juin_juillet, Moy_juin = Moy_juin,  Moy_juillet = Moy_juillet, juin1_15 = juin1_15, juin16_30=juin16_30, juil1_15 =juil1_15, juil16_31=juil16_31)
    
    temp_moyenne_estivale <- rbind(temp_moyenne_estivale , tab3)
  }}

# radiation solaire ####
sites <- unique(meteo_all$Field_site)
radiation <- data.frame()
for (i in 1:length(sites)) {
  
  site = sites[i]
  tab1 = meteo_all[which(meteo_all$Field_site == site),]
  
  years <- unique(tab1$Period_Year)
  
  for (j in 1:length(years)) {
    
    year <- years[j]
    tab2 <- tab1[which(tab1$Period_Year== year), ]  
    jun <- subset(tab2, tab2$month == 6 )
    jul <- subset(tab2, tab2$month == 7 )
    Moy_juin_juillet <- mean(c(jun$rsds, jul$rsds))
    # pr_juin <- mean(jun$pr)
    Moy_juin <- mean(jun$rsds)
    Moy_juillet <- mean(jul$rsds)
    
    juin5_juil15 <- dplyr::filter(tab2, Period_Day >= 156 & Period_Day<=196)
    juin5_juil15 <- mean(juin5_juil15$rsds)
    
    juin1_15 <- dplyr::filter(tab2, Period_Day >= 152 & Period_Day<=166)
    juin1_15 <- mean(juin1_15$rsds)
    
    juin16_30 <-  dplyr::filter(tab2, Period_Day >= 167 & Period_Day<=181)
    juin16_30<- mean(juin16_30$rsds)
    
    juil1_15 <- dplyr::filter(tab2, Period_Day >= 182 & Period_Day<=196)
    juil1_15 <- mean(juil1_15$rsds)
    
    juil16_31<- dplyr::filter(tab2, Period_Day >= 197 & Period_Day<=212)
    juil16_31 <- mean(juil16_31$rsds)
    tab3 <- data.frame(Field_site = unique(tab2$Field_site), 
                       Period_Year = unique(tab2$Period_Year),   rMoy_juin_juillet = Moy_juin_juillet, rMoy_juin = Moy_juin,  rMoy_juillet = Moy_juillet, rjuin5_juil15=juin5_juil15, rjuin1_15 = juin1_15, rjuin16_30=juin16_30, rjuil1_15 =juil1_15, rjuil16_31=juil16_31)
    
    radiation <- rbind(radiation , tab3)
  }}


#précipitations par sous-périodes####
sites <- unique(meteo_all$Field_site)
prec <- data.frame()
for (i in 1:length(sites)) {
  
  site = sites[i]
  tab1 = meteo_all[which(meteo_all$Field_site == site),]
  
  years <- unique(tab1$Period_Year)
  
  for (j in 1:length(years)) {
    
    year <- years[j]
    tab2 <- tab1[which(tab1$Period_Year== year), ]  
    
    
    juin1_15 <- dplyr::filter(tab2, Period_Day >= 152 & Period_Day<=166)
    juin1_15 <- sum(juin1_15$pr)
    
    juin16_30 <-  dplyr::filter(tab2, Period_Day >= 167 & Period_Day<=181)
    juin16_30<- sum(juin16_30$pr)
    
    juil1_15 <- dplyr::filter(tab2, Period_Day >= 182 & Period_Day<=196)
    juil1_15 <- sum(juil1_15$pr)
    
    juil16_31<- dplyr::filter(tab2, Period_Day >= 197 & Period_Day<=212)
    juil16_31 <- sum(juil16_31$pr)
    tab3 <- data.frame(Field_site = unique(tab2$Field_site), 
                       Period_Year = unique(tab2$Period_Year), pr_juin1_15 = juin1_15, pr_juin16_30=juin16_30, pr_juil1_15 =juil1_15, pr_juil16_31=juil16_31)
    
    prec <- rbind(prec , tab3)
  }}
# couverture de nuages ####
sites <- unique(meteo_all$Field_site)
nuages <- data.frame()
for (i in 1:length(sites)) {
  
  site = sites[i]
  tab1 = meteo_all[which(meteo_all$Field_site == site),]
  
  years <- unique(tab1$Period_Year)
  
  for (j in 1:length(years)) {
    
    year <- years[j]
    tab2 <- tab1[which(tab1$Period_Year== year), ]  
    jun <- subset(tab2, tab2$month == 6 )
    jul <- subset(tab2, tab2$month == 7 )
    Moy_juin_juillet <- mean(c(jun$clt, jul$clt))
    # pr_juin <- mean(jun$pr)
    Moy_juin <- mean(jun$clt)
    Moy_juillet <- mean(jul$clt)
    
    # juin1_15 <- dplyr::filter(tab2, Period_Day >= 152 & Period_Day<=166)
    # juin1_15 <- mean(juin1_15$rsds)
    # 
    # juin16_30 <-  dplyr::filter(tab2, Period_Day >= 167 & Period_Day<=181)
    # juin16_30<- mean(juin16_30$rsds)
    
    # juil1_15 <- dplyr::filter(tab2, Period_Day >= 182 & Period_Day<=196)
    # juil1_15 <- mean(juil1_15$rsds)
    # 
    # juil16_31<- dplyr::filter(tab2, Period_Day >= 197 & Period_Day<=212)
    # juil16_31 <- mean(juil16_31$rsds)
    tab3 <- data.frame(Field_site = unique(tab2$Field_site), 
                       Period_Year = unique(tab2$Period_Year),   nMoy_juin_juillet = Moy_juin_juillet, nMoy_juin = Moy_juin, nMoy_juillet = Moy_juillet 
                       # ,rjuin1_15 = juin1_15, rjuin16_30=juin16_30, rjuil1_15 =juil1_15, rjuil16_31=juil16_31
    )
    
    nuages <- rbind(nuages , tab3)
  }}
var_meteo <- dplyr::left_join(temp_moyenne_estivale, degres_jours196)
var_meteo <- dplyr::left_join(var_meteo, prec)
var_meteo <- dplyr::left_join(var_meteo, prec_acc196)
var_meteo <- dplyr::left_join(var_meteo, neige)
var_meteo <- dplyr::left_join(var_meteo, radiation)
var_meteo <- dplyr::left_join(var_meteo, nuages)
var_meteo$Period_Year <- as.integer(var_meteo$Period_Year)
var_meteo$T_last_year <- NA
new_meteo<- data.frame()
sites<- sort(unique(var_meteo$Field_site))


for (i in 1: length(sites)) {
  site <- sites[i]
  tab1<- var_meteo[which(var_meteo$Field_site == site),]
  years <- sort(unique(tab1$Period_Year))
  j=1
  year <- years[j]

  tab2 <- tab1[which(tab1$Period_Year == year),]
  new_meteo<-rbind(new_meteo, tab2)
for (j in 2:length(years)){
  year <- years[j]
  last_year <- years[j-1]
  tab2 <- tab1[which(tab1$Period_Year == year),]
  tab3<- tab1[which(tab1$Period_Year == last_year),]
  tab2$T_last_year <- tab3$Moy_juin_juillet
  new_meteo<-rbind(new_meteo, tab2)
} }

head(new_meteo)

return(new_meteo)
}


