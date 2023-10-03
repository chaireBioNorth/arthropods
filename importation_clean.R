
#### Importation des données et formatage de base ####

library(dplyr)
setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/")
# ...importer jeu de données Élise : "ARTRHO-ALL ----

arthro_all <- read.table("DATA/FINAL_DATA/arthro_all_clean.txt")

#take out Bylot to replace it with with the up-to-date dataset
arthro_all <- dplyr::filter(arthro_all, Field_site !="Bylot")
# résultat : biomasse par groupe par trappe + nb of traps + trap_days

# ......ajout Bylot  ----

BAT<- read.table("DATA/FINAL_DATA/BD_arthropodes2018.txt")

arthro_all <- dplyr::full_join(arthro_all, BAT)

source('scripts/daily_biomass_fun.R')
arthro_all2<-daily_biomass(arthro_all)
arthro_all <- arthro_all2
# importer d'autres sites dans " ARTHRO_ALL" ----

# .....[ Taimyr ] ----

TA <- read.table("DATA/FINAL_DATA/Taimyr_brut.txt", header = TRUE, sep = "\t", dec = ",", blank.lines.skip=TRUE)

source('scripts/import_TA_fun.R')
dfTA <- import_TA(data=TA)

# .....[ Igloolik ] ----
IG <- read.table("DATA/FINAL_DATA/igloolik_Arthropodes_V3.txt", header = TRUE, sep = "\t", dec = ",") 

source('scripts/import_IG_fun.R')
dfIG <- import_IG(data=IG)


# .....[ Churchill ] ----


CH<- read.table("DATA/FINAL_DATA/2010_2011 CHUR insect biomass.txt", header = TRUE, sep = "\t", dec = ",")
source('scripts/import_CH_fun.R')
dfCH <- import_CH(data=CH)

#....[ Zackenberg ]----
ZA<- read.table("DATA/FINAL_DATA/zack_top_standard.txt")

source('scripts/import_ZA_fun.R')
dfZA<-import_ZA(data=ZA)

# ....[ Akimiski ]----
AK<- read.table(file = "DATA/FINAL_DATA/akimiski_std.txt")
source('scripts/import_AK_fun.R')
dfAK <- import_AK(data=AK)

# .... [ ASDN ] ----
asdn_data <- read.table("DATA/FINAL_DATA/ASDN_Invert_biomass.txt", sep = "\t", header = T, dec = ".")
source('scripts/import_ASDN_fun.R')
dfASDN <- import_ASDN(asdn_data)

# .... [ Utqiagvik ] ----

utqiagvik <- read.csv("DATA/FINAL_DATA/Utqiagvik_invertebrate_biomass.csv")
source('scripts/import_utq_fun.R')
dfUT <- import_utq_fun(data=utqiagvik)


# ....[ Hochstetter ] ----
hoch <- read.table("DATA/FINAL_DATA/Hochstetter_arthropod_biomass.txt", header= T, sep="\t")
source('scripts/import_HOCH.R')
dfHO<-import_HO(hoch)
 
# arthro_all <-backup
backup <- arthro_all

# ...fusion ARTRHO_ALL + CH + IG + TA + ZA----
arthro_all <- dplyr::full_join(arthro_all, dfCH)
arthro_all <- dplyr::full_join(arthro_all, dfIG)
arthro_all <- dplyr::full_join(arthro_all, dfTA)
arthro_all <- dplyr::full_join(arthro_all, dfZA)
arthro_all <- dplyr::full_join(arthro_all, dfAK)
arthro_all <- dplyr::full_join(arthro_all, dfASDN)
arthro_all <- dplyr::full_join(arthro_all, dfUT)
arthro_all <- dplyr::full_join(arthro_all, dfHO)
head(arthro_all)
str(arthro_all)

# .... ménage taxons ----

source('scripts/clean_fun.R')
arthro_all_clean <- clean(data=arthro_all)
check_no_biomass<- dplyr::filter(arthro_all_clean, is.na(Arthropod_Biomass))
### ici 326 obs sont supprimées... pourraient en partie être récupérées !!! ----
arthro_all <- dplyr::filter(arthro_all_clean, !is.na(Arthropod_Biomass))

## VERSION AVEC SOMMES GROSSIÈRES par habitat ####
source('scripts/sommes_fun.R')

mg_trappe_jour_hab <- somme_hab(data=arthro_all)

#commande pour avoir le nombre d'échantillons : divisé par trap_days à cause de la réplication des lignes par trap_days dans somme_hab
# mg_trappe_jour_hab <- read.table ("DATA/FINAL_DATA/mg_trappe_jour_habitat_sep.txt")
#  sum(mg_trappe_jour_hab$nb_of_trap/mg_trappe_jour_hab$trap_days, na.rm=T)

write.table(mg_trappe_jour_hab, file = "DATA/FINAL_DATA/mg_trappe_jour_habitat_sep.txt")

   # mg_trappe_jour_hab <-read.table("DATA/FINAL_DATA/mg_trappe_jour_habitat_sep.txt")
mg_trappe_jour_site <- somme_site_jour(data=mg_trappe_jour_hab)

write.table(mg_trappe_jour_site, file = "DATA/FINAL_DATA/mg_trappe_jour_moy_des_habs.txt")

   mg_trappe_jour_site <- read.table("DATA/FINAL_DATA/mg_trappe_jour_moy_des_habs.txt")

source("scripts/extract_param_fun.R")
param <- extract_param(data=mg_trappe_jour_site)

#retirer sites aux saisons trop courtes (25 jours et moins) (ajusté post HOCH)

 param <-  param[-which(param$Field_site == "Igloolik" & param$Period_Year == "2015"),]

 # param <-  param[-which(param$Field_site == "Canning" & param$Period_Year == "2010"),]
 param <-  param[-which(param$Field_site == "Mackenzie" & param$Period_Year == "2010"),]
 param[param=="Taimyr"]<-"Medusa"


# ...METEO ----
meteo <- read.table("DATA/tables_intermediaires/table_climate_CH_SO_ZA_TA.txt", header =  T )

source('scripts/import_meteo_fun.R')
var_meteo<-import_meteo(meteo)
var_meteo$Snowmelt_day[which(var_meteo$Snowmelt_day<106)]<-NA
write.table(var_meteo, "DATA/FINAL_DATA/var_meteo.txt" )
read.table("DATA/FINAL_DATA/var_meteo.txt")
#### Fusion meteo + arthro ----
head(param_plus)
param_plus <- left_join(param, var_meteo)


#créer un ordre permanent des sites par Temp moyenne
# Moyennes de température par site
sites<- unique(param_plus$Field_site)
tabmoy<-data.frame()
for (i in 1:length(sites)) {
  site <- sites[i]
  tab1 <- param_plus[which(param_plus$Field_site==site),]
  TMOY <- mean(tab1$Moy_juin_juillet, na.rm=T)
  tab <- data.frame(Field_site=site, TMOY=TMOY)
  tabmoy<- rbind(tabmoy, tab)
}
param_plus <- left_join(param_plus,tabmoy)
# param_plus$TMOY <- NA
# param_plus$TMOY[which(param_plus$Field_site=="Utqiagvik")]<-1
# param_plus$TMOY[which(param_plus$Field_site=="Zackenberg")]<-2
# param_plus$TMOY[which(param_plus$Field_site=="Alert")]<-3
# param_plus$TMOY[which(param_plus$Field_site=="Ikpikpuk")]<-4
# param_plus$TMOY[which(param_plus$Field_site=="Taimyr")]<-5
# param_plus$TMOY[which(param_plus$Field_site=="Prudhoe")]<-6
# param_plus$TMOY[which(param_plus$Field_site=="Bylot")]<-7
# param_plus$TMOY[which(param_plus$Field_site=="Chipp")]<-8
# param_plus$TMOY[which(param_plus$Field_site=="Igloolik")]<-9
# param_plus$TMOY[which(param_plus$Field_site=="Canning")]<-10
# param_plus$TMOY[which(param_plus$Field_site=="Colville")]<-11
# param_plus$TMOY[which(param_plus$Field_site=="Herschel")]<-12
# param_plus$TMOY[which(param_plus$Field_site=="Mackenzie")]<-13
# param_plus$TMOY[which(param_plus$Field_site=="Southampton")]<-14
# param_plus$TMOY[which(param_plus$Field_site=="Nome")]<-15
# param_plus$TMOY[which(param_plus$Field_site=="Churchill")]<-16
# param_plus$TMOY[which(param_plus$Field_site=="Krusenstern")]<-17
# param_plus$TMOY[which(param_plus$Field_site=="Akimiski")]<-18

# param_plus<- param

param_plus$Field_site<-as.character(param_plus$Field_site)
#param_plus$Field_site[param_plus$Field_site== "Taimyr"] <- "Medusa"
param_plus$Field_site<-as.factor(param_plus$Field_site)
write.table (param_plus, file = "DATA/FINAL_DATA/param_plus.txt")

