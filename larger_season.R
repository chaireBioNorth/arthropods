# simplification du script lissage_moyenne_mobile2.R -> juste habitats ensemble et sans les sections éliminées
#importation des données
library(dplyr)
# [[[[ MOYENNE DES HABITATS ]]]] ----
data <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/mg_trappe_jour_moy_des_habs.txt")


data <- dplyr::filter(data, !is.na(mg_jour_trappe))

AK <-subset(data, Field_site == "Akimiski")
AL <- subset(data, Field_site == "Alert")
BY <- subset(data, Field_site == "Bylot")
CH <- subset(data, Field_site == "Churchill")
IG <- subset(data, Field_site == "Igloolik")
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
HO <- subset(data, Field_site == "Hochstetter")


jeux<- list(AK,AL, UT, BY, CA, CH,CO, CP, HS, HO ,IG,IK, KR, MA, NO, PR, SO, TA, ZA)



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
    long <- range[2]-range[1]
    
    tab3 <- data.frame (Field_site = unique(tab2$Field_site), Period_Year = unique(tab2$Period_Year), deb = range[1], fin= range[2], long=long)  
    
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
subs_southampton2007 <- dplyr::filter(Southampton_2007, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
subs_utqiagvik2010 <- dplyr::filter(Utqiagvik_2010, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )

# subs_zack1998 <- dplyr::filter(Zackenberg_1998,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )

# subs_colville2011 <- dplyr::filter(Colville_2011,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )
# subs_zack2010 <- dplyr::filter(Zackenberg_2010,Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
# subs_canning2012 <- dplyr::filter(Canning_2012,Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin )


#prob début
subs_akimiski2009 <- dplyr::filter(Akimiski_2009, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
subs_canning2011 <- dplyr::filter(Canning_2011, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )

subs_bio <- rbind(subs_bio, subs_bylot2005, subs_utqiagvik2011,subs_mackenzie2011, subs_southampton2007, subs_utqiagvik2010, subs_canning2011,subs_akimiski2009)



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


# période élargie +/- 15 ----
tab_apercu2 <- data.frame()


sites <- unique(tab_apercu$Field_site)
for (i in 1:length(sites)) {
  site <- sites[i]
  tab1 <- tab_apercu[which(tab_apercu$Field_site == site),]
  
  years <- unique(tab1$Period_Year)  
  
  for (j in 1:length(years)) {
    year <- years[j]
    
    tab2 <- tab1[which(tab1$Period_Year == year),]
    tab2$deb_pic <- tab2$Peak_date_raw - 15
    tab2$fin_pic <- tab2$Peak_date_raw + 15
    tab2$diff_deb <- tab2$deb_pic - tab2$deb
    tab2$diff_fin <- tab2$fin - tab2$fin_pic
    
    tab_apercu2 <- rbind(tab_apercu2, tab2)
  }}


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
subs_krusenstern2011 <- dplyr::filter(Krusenstern_2011, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)
subs_canning2010 <- dplyr::filter(Canning_2010, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)
subs_southampton2011 <- dplyr::filter(Southampton_2011, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)
subs_taimyr1996 <- dplyr::filter(Taimyr_1996, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)
subs_igloolik2014 <- dplyr::filter(Igloolik_2014, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)
subs_southampton2008 <- dplyr::filter(Southampton_2008, Period_Day >= deb_pic + diff_fin & Period_Day <= fin_pic + diff_fin)


#prob début
subs_mackenzie2010 <- dplyr::filter(Mackenzie_2010, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
subs_alert2008 <- dplyr::filter(Alert_2008, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
subs_utqiagvik2015 <- dplyr::filter(Utqiagvik_2015, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
subs_churchill2010 <- dplyr::filter(Churchill_2010, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )
subs_bylot2016 <- dplyr::filter(Bylot_2016, Period_Day >= deb_pic - diff_deb & Period_Day <= fin_pic - diff_deb )

subs_bio <- rbind(subs_bio, subs_krusenstern2011,subs_canning2010,subs_southampton2011,subs_taimyr1996,subs_igloolik2014,subs_southampton2008,subs_mackenzie2010,subs_alert2008,subs_utqiagvik2015,subs_churchill2010,subs_bylot2016)



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
    tab3 <- data.frame (Field_site = unique(tab2$Field_site), Period_Year = unique(tab2$Period_Year), somme_sais_15 = somme)  
    
    auc_raw<- rbind( auc_raw, tab3)
  }
}
auc_15 <- auc_raw
param15 <- left_join(param,auc_15, by = c("Field_site", "Period_Year"))
param15$diff <- param15$somme_sais_15-param15$somme_sais_raw

#corrélation ----
param15 <-dplyr::filter(param15, !is.na(somme_sais_15))
param15 <-dplyr::filter(param15, !is.na(somme_sais_raw))
cor.test(param15$somme_sais_raw, param15$somme_sais_15, method="pearson", alternative = "t")

#### Fusion meteo + arthro ----


param_plus15 <- left_join(param15, var_meteo)
param_plus15 <- dplyr::filter(param_plus15, !is.na(dj196))
ggplot(param_plus15, aes(x=dj196, y=diff)) + geom_point(aes(col=Field_site), size =2)


ggplot(param_plus15, aes(x=somme_sais_raw, y=diff)) + geom_point(aes(col=Field_site), size =2)

ggplot(param_plus15, aes(x=dj196, y=somme_sais_15)) + geom_point(aes(col=Field_site), size =2)

ggplot(param_plus15, aes(x=dj196, y=somme_sais_raw)) + geom_point(aes(col=Field_site), size =2)

ggplot(param_plus15, aes(x=somme_sais_raw, y=somme_sais_15)) + geom_point(aes(col=Field_site), size =2)
param_plus15$
# Sélection modèle ----
#ajout colonne w
data<- param_plus15
  field_sites <- unique(data$Field_site)
m_data <- data.frame()

for (i in 1:length(field_sites)){
  fs <- field_sites[i]
  tab1<- data[which(data$Field_site==fs),]
  tab1$w <- 1/length(tab1$Period_Year)
  m_data <- rbind(m_data, tab1)
}



## Modèles pondérés ----
data <- m_data

data <- dplyr::filter(data, !is.na(Snowmelt_day))
m_nul <- lm(somme_sais_raw~1, data=data, weights=w)
m_base <- lm(somme_sais_raw~dj196, data = data, weights = w)
m_lprsn <- lm(somme_sais_raw~dj196 + pr196 + Snowmelt_day, data = data, weights = w)
m_lsnow <-lm(somme_sais_raw~dj196  + Snowmelt_day, data = data, weights = w)
m_lpr <- lm(somme_sais_raw~dj196  + pr196, data = data, weights = w)
m_lrad <-lm(somme_sais_raw~dj196 + rMoy_juin_juillet, data = data, weights = w)
#m_snow<-lm(somme_sais_raw~Snowmelt_day, data = data, weights = w)

mods_simple<- model.sel(m_nul,m_base, m_lrad, m_lpr, m_lprsn, m_lsnow
                        #, m_snow
                        )

# inflection point analysis ----
seuil <- seq(10,530, 10) 
m_data$id <- seq(1, length(m_data$Field_site), 1)
modeles <-list()
for (i in 1:length(seuil)){
  data<- m_data
  data$x <- NA
  s<- seuil[i]
  tab <- data.frame()
  for (j in 1:length(data$id)){
    id <- data$id[j]
    tab1 <- data[which(data$id == id),]
    if (tab1$dj196<=s){
      tab1$x <- tab1$dj196
    }else{ 
      tab1$x <- s
    }
    tab <- rbind(tab, tab1)
    
  }
  mod <- lm(somme_sais_raw~x, data = tab, weights = w)
  
  modeles[[i]]<- assign(paste0("mod",s), mod) 
}

sel_tout <- model.sel(modeles)

# modele53 = modèle de base (aucune donnée changée)
# modele12 = bon modèle d'inflection

m_nul <- lm(somme_sais_raw~1, data=tab, weights=w)
mod_finaux <- list(m_nul, mod120, mod530)
sel <- model.sel(mod_finaux)
