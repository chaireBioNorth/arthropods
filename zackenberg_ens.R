setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_zack")
zack<- read.table("zackenberg.txt", header = TRUE, sep = "\t")
#pour fonction year()
library(lubridate)
library(WriteXLS)
library(tidyr)
#retirer plotID Art1 (window trap, selon BioBasis)
zack <- zack[-which(zack$Plot.ID == "Art1"),]

#gestion date ----
zack$Date <- as.character(zack$Date)
zack$Date <- as.Date(zack$Date, format= "%Y-%m-%d" )
zack$Period_Year <- year(zack$Date)
ref2 <- paste(zack$Period_Year, "-01-01", sep = "")
ref2 <- as.Date(ref2)

zack$Period_Day <- (zack$Date - ref2)+1
zack$Period_Day <- as.integer(zack$Period_Day)
test2013 <- zack[which(zack$Period_Year == "2013"),]

data <- zack


test <- gather(data, key = "IDtrap", value= "trap_days", c(Days.A.or.A.D,Days.B, Days.C, Days.D, Days.E, Days.F, Days.G, Days.H ))

test$IDtrap <- substring(test$IDtrap, 6,13)
test <- test[,-c(9:16)]
test <- dplyr::filter(test, !is.na(trap_days))
test<- dplyr::filter(test, trap_days!="0" & trap_days != "-9999")
# Gestion colonnes ----
test2<- gather(data, key = "IDtrap", value="Abundance", c(A.or.A.D, B, C, D, E, F, G, H))
test2 <- test2[,-c(4:11)]
ultimate_test <- merge(test, test2)

#enlever les lignes dupliquées et vides
nodupli <- ultimate_test[!duplicated(ultimate_test),]
nodupli$Abundance[nodupli$Abundance == -9999] <- NA

zack_prefinal <- nodupli


#enlever les non-arthropodes ----
zack_prefinal <- zack_prefinal[which(zack_prefinal$Phylum== "Arthropoda"),]

zack_prefinal$Taxon <- paste(zack_prefinal$Order, zack_prefinal$Family, zack_prefinal$Genus, zack_prefinal$Species, sep="." )
# write.table(zack_prefinal, file = "zack_pre-standard.txt")




# Gestion taxons ----
taxons <- as.data.frame(unique(zack_prefinal$Taxon))

tax <- separate(data = taxons, col =1, into = c("Order", "Family", "Genus", "Species"), sep = "\\.")


library(WriteXLS)               
WriteXLS(tax, ExcelFileName = "taxons_zackenberg.xls")



# Longueur moyenne par taxon -----

ZAClength <- read.table("ZAC daily arthropod data 2007.txt", header = T, sep = "\t", dec = ",")

plot(ZAClength$Family, ZAClength$Size, las=2)
unique(ZAClength$Order)

moyenne <- data.frame()
ord <- (unique(ZAClength$Order ))
for (h in 1:length(ord)) {

tab1 <- ZAClength[which(ZAClength$Order == ord[h]),]  
  fam <- unique(tab1$Family)

for (i in 1:length(fam)) {
  
tab2 <- tab1[which(tab1$Family == fam[i]),]
moy <- mean(tab2$Size)
sd <- sd(tab2$Size)
df <- data.frame(Order = unique(tab2$Order), Family = unique(tab2$Family), Moyenne = moy, SD = sd)  
moyenne <- rbind(moyenne, df)  
}}
WriteXLS(moyenne, "longueurs_moyennes_ZAC2007.xls")



# Longueur mediane par taxon -----


mediane <- data.frame()
ord <- (unique(ZAClength$Order ))
for (h in 1:length(ord)) {
  
  tab1 <- ZAClength[which(ZAClength$Order == ord[h]),]  
  fam <- unique(tab1$Family)
  
  for (i in 1:length(fam)) {
    
    tab2 <- tab1[which(tab1$Family == fam[i]),]
    med <- median(tab2$Size)
   
    df <- data.frame(Order = unique(tab2$Order), Family = unique(tab2$Family), mediane = med)  
    mediane <- rbind(mediane, df)  
  }}

WriteXLS(mediane, "longueurs_medianes_ZAC2007.xls")

#pour connaître l'occurence d'un taxon ----
length(which(zack_prefinal$Order =="Lepidoptera" & zack_prefinal$Family == "Lepidoptera larvae"))
zack_prefinal[which(zack_prefinal$Family=="Hymenoptera larvae"),]
length(which(zack_prefinal$Order =="Siphonaptera"))
aranea <- zack_prefinal[which(zack_prefinal$Order=="Aranea"),]
liny <- aranea[which(aranea$Family== "Linyphiidae"),]
araNA <- dplyr::filter(aranea, is.na(Family))

length(which(hymeno$Family=="NA"))
length(which(sipho$Abundance == 0))
length(which(zack_prefinal$Family =="Tipulidae larvae"))
hymeno <- dplyr::filter(ZAClength, Order == "HYME")
hymeno <- dplyr::filter(hymeno, Family != "BOMB")
unique(hymeno$Family)

t =table(hymeno$Family)
w = as.data.frame(t)
x <- data.frame(unique(hymeno$Family), c(19, 173, 8, 2,  5))
names(x) <- c("Fam", "Freq") 

# Hymeno ----
moy_pond <- mean(hymeno$Size)
para <-dplyr::filter(hymeno, Family == "PARA")
ichn<-dplyr::filter(hymeno, Family == "ICHN")
miri <-dplyr::filter(hymeno, Family == "MIRI")
tent <-dplyr::filter(hymeno, Family == "TENT")
para_size <- mean(para$Size)
ichn_size <-mean(ichn$Size)
miri_size <-mean(miri$Size)
tent_size <-mean(tent$Size)
sd(hymeno$Size)
moy_gr <- mean(c(para_size, ichn_size, miri_size, tent_size))
med <- median(hymeno$Size)
c(moy_pond, moy_gr, med)

#moyenne pour les hémiptères ----
#subset d'hémiptères
hemi <- ZAClength[which(ZAClength$Order == "HEMI"),]
mean(hemi$Size)
sd(hemi$Size)
median(hemi$Size)

#vérification variation saisonnière ----
ord <- (unique(ZAClength$Order ))
for (h in 1:length(ord)) {
  
  tab1 <- ZAClength[which(ZAClength$Order == ord[h]),]  
  fam <- unique(tab1$Family)
  
  for (i in 1:length(fam)) {
    
    tab2 <- tab1[which(tab1$Family == fam[i]),]
  plot(tab2$Day.of.year, tab2$Size, main= paste(unique(tab2$Order),unique(tab2$Family), sep = "-"))
  }}
## évaluation visuelle == ok



# Équations Zackenberg ----
setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/data_zack")
library(dplyr)
equations <- read.table("equations_zack.txt", header = T, sep = "\t")
# supprimer lignes inutiles
equations <- equations[-23,]

# Appliquer les équations longueur-masse ----

#créer les équations ----
#ici il y a la version fonction(L,N) et la fonction où L est remplacé par la longueur moyenne de Zack2007 (Jeroen)
#funLiny <-function(N){ (-0.0096+ 0.0081*exp(0.0975*2.29878))*N}
funLiny <-(-0.0096+ 0.0081*exp(0.0975*2.29878))
# funLyco <-function(N){ (-0.0096+ 0.0081*exp(0.0975*6.614554))*N}
funLyco <-(-0.0096+ 0.0081*exp(0.0975*6.614554))
#funThom <-function(N){ (-0.0017 + 1.2327*10-6 *exp(1.3726 *3.927765))*N}
funThom <- (-0.0017 + 1.2327*10^(-6) *exp(1.3726 * 3.927765))
funThom_A <- 0.028*(3.927765)^2.761 #équation pour aranea
#donne une valeur négative, comment ça?
#funCocc <-function(N){ (0.045(5)^2.143)*N}
funCocc <- (0.045*5^2.143)
# funCall <-function(N){ (exp(-3.374)*(10.764706^2.158))*N}
funCall <- (exp(-3.374)*(10.764706^2.158))
# funChir <-function(N){ 0.049*N}
funChir <- 0.049
# funCuli <-function(N){ 0.67*N}
funCuli <-0.67
# funMusc <-function(N){(-0.002+ 0.0011*exp(0.1937*6.333018))*N}
funMusc <-(-0.002+ 0.0011*exp(0.1937*6.333018))
# funPhor <-function(N){exp(-0.8503+0.69325*L +(-0.01762*2.090909^2))*N}
funPhor <-exp(-0.8503+0.69325*2.090909 +(-0.01762*2.090909^2))
# funScia <-function(N){ 0.066*N}
funScia <-0.066
# funSyrp <-function(N){(exp(-8.503+0.6935*L+(-0.017613*6.615385^2)))*N}
funSyrp <-(exp(-8.503+0.6935*6.615385+(-0.017613*6.615385^2)))
# funTipu <-function(N){(0.0008 + 0.0003 *exp(0.2055 *15.189189))*N}
funTipu <- (0.0008 + 0.0003 *exp(0.2055 *15.31707317))
# funTric <-function(N){ 0.27*N}
funTric <- 0.27
# funIchn <-function(N){ (0.05*(4.1875)^3.013)*N}
funIchn <-(0.05*(4.1875)^3.013)
# funTent <-function(N){(0.008(12)^2.883)*N}
funTent <-(0.008*12^2.883)
# funMyce <-function(N){0.27*N}
funMyce <-0.27


#longueurs d'autres sources ou techniques

funLyge <-0.01*(5)^2.858
funHemi <- 0.01*(3.285714)^2.858
funLepi <- 0.0011* exp(0.1495*14.6548672566372)
funHyme1 <- -0.0033+ 0.0018*exp(0.1577*5.965426) #moyenne pondérée
funHyme2 <- -0.0033+ 0.0018*exp(0.1577*5.494707) #moyenne des groupes
funHyme3 <- -0.0033+ 0.0018*exp(5577*5) #médiane
funDict <- 0.028*(2.5)^2.761
#ajout des équations "résolues" dans le dataframe des familles ----
equations$eqN <- c(funLiny, funLyco, funThom_A, funCocc, funCall, funChir, funCuli, funMusc, funPhor, funScia,
                   funSyrp, funTipu, funTric, funIchn, funTent, funMyce, funDict, funDict, funLyge, funHemi, funLepi, funHyme1)

#un peu de ménage (enlever le doublon (dicty) et les colonnes superficielles)
equ_simple <- equations[-18,-c(3:5)]

# data principal zack
zack_data <- read.table("zack_pre-standard.txt")

# ajout du tableau des familles au tableau principal ----

wow <- left_join(zack_data,equ_simple, by=c("Order", "Family"))

#on multiplie N (abondance) avec équation résolue incluant la longueur (si applicable)
#donne biomasse totale par espèce/échantillon
wow$biomasse <- wow$Abundance * wow$eqN

# extraire des biomasses par individu de Arthro_ALL ----
arthro_all <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/raw/ARTHROPOD_ALL.txt", header = TRUE, sep = "\t", dec = ",")
df <- data.frame(matrix(ncol=5, nrow=0))
x <- c("Order","Family", "Genus", "moy_ind", "med_ind")
colnames(df) <- x
tax_bio <- read.table("taxons_biomasse.txt", header = T, sep = "\t")
tax <- tax_bio$Family

# biomasse sac d'oeufs
egg <- arthro_all[which(arthro_all$Stage == "Egg sac"),]
egg<- dplyr::filter(egg, !is.na(Arthropod_Biomass))
egg<-dplyr::filter(egg, !is.na(Arthropod_Count))
egg <- dplyr::filter(egg, Arthropod_Biomass > 0)
masse_ind <- egg$Arthropod_Biomass/egg$Arthropod_Count
moy_egg <- mean(masse_ind)
med_egg <- median(masse_ind)

# biomasse cecidomyiidae -- bon

ceci <- dplyr::filter(arthro_all, Arthropod_Family == "Cecidomyiidae")
ceci<- dplyr::filter(ceci, !is.na(Arthropod_Biomass))
ceci<-dplyr::filter(ceci, !is.na(Arthropod_Count))
ceci <- dplyr::filter(ceci, Arthropod_Biomass > 0)
ceci$masse_ind<- ceci$Arthropod_Biomass/ceci$Arthropod_Count
plot(ceci$Field_site, ceci$masse_ind)
plot(ceci$Period_Day, ceci$masse_ind)
moy_ceci <- mean(ceci$masse_ind)
med_ceci <- median(ceci$masse_ind)
sd_ceci <- sd(ceci$masse_ind)
range(ceci$masse_ind)
hist(ceci$masse_ind)


# biomasse ceratopogonidae
cera <- dplyr::filter(arthro_all, Arthropod_Family == "Ceratopogonidae")
cera<- dplyr::filter(cera, !is.na(Arthropod_Biomass))
cera<-dplyr::filter(cera, !is.na(Arthropod_Count))
cera <- dplyr::filter(cera, Arthropod_Biomass > 0)
cera$masse_ind <- cera$Arthropod_Biomass/cera$Arthropod_Count
plot(cera$Field_site, cera$masse_ind)
plot(cera$Period_Day, cera$masse_ind)
moy_cera <- mean(cera$masse_ind)
se_cera <- sd(cera$masse_ind)
med_cera <- median(cera$masse_ind)
hist(cera$masse_ind)

#biomasse empididae -- pas top
empi <- dplyr::filter(arthro_all, Arthropod_Family == "Empididae")
empi<- dplyr::filter(empi, !is.na(Arthropod_Biomass))
empi<-dplyr::filter(empi, !is.na(Arthropod_Count))
empi <- dplyr::filter(empi, Arthropod_Biomass > 0)
empi$masse_ind <- empi$Arthropod_Biomass/empi$Arthropod_Count
plot(empi$Field_site, empi$masse_ind)
plot(empi$Period_Day, empi$masse_ind)
moy_empi <- mean(empi$masse_ind)
se_empi <- sd(empi$masse_ind)
hist(empi$masse_ind)
med_empi <- median(empi$masse_ind)

#biomasse scathophagidae -- bon
scat <- dplyr::filter(arthro_all, Arthropod_Family == "Scatophagidae")
scat<- dplyr::filter(scat, !is.na(Arthropod_Biomass))
scat<-dplyr::filter(scat, !is.na(Arthropod_Count))
scat <- dplyr::filter(scat, Arthropod_Biomass > 0)
scat$masse_ind <- scat$Arthropod_Biomass/scat$Arthropod_Count
plot(scat$Field_site, scat$masse_ind)
plot(scat$Period_Day, scat$masse_ind)
med_scat <- median(scat$masse_ind)
moy_scat <- mean(scat$masse_ind)
se_scat <- sd(scat$masse_ind)
hist(scat$masse_ind)

#biomasse tachinidae -- correct
tach <- dplyr::filter(arthro_all, Arthropod_Family == "Tachinidae")
tach<- dplyr::filter(tach, !is.na(Arthropod_Biomass))
tach<-dplyr::filter(tach, !is.na(Arthropod_Count))
tach <- dplyr::filter(tach, Arthropod_Biomass > 0)
tach$masse_ind <- tach$Arthropod_Biomass/tach$Arthropod_Count
plot(tach$Field_site, tach$masse_ind)
plot(tach$Period_Day, tach$masse_ind)
med_tach <- median(tach$masse_ind)
moy_tach <- mean(tach$masse_ind)
se_tach <- sd(tach$masse_ind)
hist(tach$masse_ind)

#biomasse braconidae  #PAS BON POUR LA MOYENNE - AKIMISKI DANS LE CHAMP
brac <- dplyr::filter(arthro_all, Arthropod_Family == "Braconidae")
brac<- dplyr::filter(brac, !is.na(Arthropod_Biomass))
brac<-dplyr::filter(brac, !is.na(Arthropod_Count))
brac <- dplyr::filter(brac, Arthropod_Biomass > 0)
brac$masse_ind <- brac$Arthropod_Biomass/brac$Arthropod_Count
plot(brac$Field_site, brac$masse_ind)
plot(brac$Period_Day, brac$masse_ind)
med_brac<- median(brac$masse_ind)
moy_brac <- mean(brac$masse_ind)
se_brac <- sd(brac$masse_ind)
hist(brac$masse_ind)

#biomasse scelionidae
scel <- dplyr::filter(arthro_all, Arthropod_Family == "Scelionidae")
scel<- dplyr::filter(scel, !is.na(Arthropod_Biomass))
scel<-dplyr::filter(scel, !is.na(Arthropod_Count))
scel <- dplyr::filter(scel, Arthropod_Biomass > 0)
scel$masse_ind <- scel$Arthropod_Biomass/scel$Arthropod_Count
plot(scel$Field_site, scel$masse_ind)
plot(scel$Period_Day, scel$masse_ind)
med_scel <- median(scel$masse_ind)
moy_scel <- mean(scel$masse_ind)
se_scel <- sd(scel$masse_ind)
range(scel$masse_ind)


#biomasse thysanoptera  --- juste Herschel, sinon semble correct
thys <- dplyr::filter(arthro_all, Arthropod_Order == "Thysanoptera")
thys<- dplyr::filter(thys, !is.na(Arthropod_Biomass))
thys<-dplyr::filter(thys, !is.na(Arthropod_Count))
thys <- dplyr::filter(thys, Arthropod_Biomass > 0)
thys$masse_ind <- thys$Arthropod_Biomass/thys$Arthropod_Count
plot(thys$Field_site, thys$masse_ind)
plot(thys$Period_Day, thys$masse_ind)
med_thys <- median(thys$masse_ind)
moy_thys <- mean(thys$masse_ind)
se_thys <- sd(thys$masse_ind)
range(thys$masse_ind)

#biomasse larves diptera
dipt <- dplyr::filter(arthro_all, Arthropod_Order == "Diptera")
dipt_larv <- dplyr::filter(dipt, Stage == "Larva")
dipt_larv<- dplyr::filter(dipt_larv, !is.na(Arthropod_Biomass))
dipt_larv<-dplyr::filter(dipt_larv, !is.na(Arthropod_Count))
dipt_larv <- dplyr::filter(dipt_larv, Arthropod_Biomass > 0)
dipt_larv$masse_ind <- dipt_larv$Arthropod_Biomass/dipt_larv$Arthropod_Count
plot(dipt_larv$Field_site, dipt_larv$masse_ind)
plot(dipt_larv$Period_Day, dipt_larv$masse_ind)
med_dipt_larv <- median(dipt_larv$masse_ind)
moy_dipt_larv <- mean(dipt_larv$masse_ind)
se_dipt_larv <- sd(dipt_larv$masse_ind)
range(dipt_larv$masse_ind)

#mesures/biomasse agromyzidae
agro <- dplyr::filter(arthro_all, Arthropod_Family == "Agromyzidae")
agro<- dplyr::filter(agro, !is.na(Arthropod_Biomass))
agro<-dplyr::filter(agro, !is.na(Arthropod_Count))
agro <- dplyr::filter(agro, Arthropod_Biomass > 0)
agro$masse_ind <- agro$Arthropod_Biomass/agro$Arthropod_Count
plot(agro$Field_site, agro$masse_ind)
plot(agro$Period_Day, agro$masse_ind)
med_agro <- median(agro$masse_ind)
moy_agro <- mean(agro$masse_ind)
se_agro <- sd(agro$masse_ind)
range(agro$masse_ind)

#mesures/biomasse anthomyiidae
anth <- dplyr::filter(arthro_all, Arthropod_Family == "Anthomyiidae")
anth<- dplyr::filter(anth, !is.na(Arthropod_Biomass))
anth<-dplyr::filter(anth, !is.na(Arthropod_Count))
anth <- dplyr::filter(anth, Arthropod_Biomass > 0)
anth$masse_ind <- anth$Arthropod_Biomass/anth$Arthropod_Count
plot(anth$Field_site, anth$masse_ind)
plot(anth$Period_Day, anth$masse_ind)
med_anth <- median(anth$masse_ind)
moy_anth <- mean(anth$masse_ind)
se_anth <- sd(anth$masse_ind)
range(anth$masse_ind)




moyennes <- c(moy_egg, moy_ceci, moy_cera, moy_empi, moy_scat, moy_tach, moy_brac, moy_scel, moy_thys, moy_dipt_larv,moy_dipt_larv, moy_agro, moy_anth)
medianes <-c(med_egg, med_ceci, med_cera, med_empi, med_scat, med_tach, med_brac, med_scel, med_thys, med_dipt_larv, moy_dipt_larv, med_agro, med_anth)
tax_bio$moy_ind <- moyennes
tax_bio$med_ind <- medianes
# tax_bio <- tax_bio[,-(3:10)]


wow2 <- left_join(wow,tax_bio, by=c( "Order","Family"))

#séparation en groupes pour pouvoir faire le calcul de biomasse
groupe_1 <- wow2[which(wow2$biomasse > 0),] # déjà calculé
groupe_2 <- wow2[-which(wow2$biomasse > 0),] # à calculer
groupe_2$biomasse <- groupe_2$Abundance*groupe_2$med_ind

wow2 <- rbind(groupe_1, groupe_2)

# ménage : retrait de tous les taxons éliminés (voir doc excel pour justifications) ----
wow_clean  <- wow2[-which(wow2$Order == "Lepidoptera" & wow2$Family !="Lepidoptera larvae"),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Acari"),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Collembola"),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Ostracoda"),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Psocoptera"),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Siphonaptera"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Dictynidae juv."),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Linyphiidae juv."),]     
wow_clean <- wow_clean[-which(wow_clean$Family == "Thomisidae juv."),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Lycosidae juv."),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Coccinellidae larvae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Brachycera larvae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Dipetera larvae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Faniidae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Heleomyzidae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Tipulidae larvae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Apidae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Ceraphronidae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Hymenoptera larvae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Megaspilidae"),]
wow_clean <- wow_clean[-which(wow_clean$Family == "Symphyta larvae"),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Diptera" & is.na(wow_clean$Family) ),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Aranea" & is.na(wow_clean$Family) ),]
wow_clean <- wow_clean[-which(wow_clean$Order == "Aranea" & wow_clean$Family == "unidentified"),]
wow_cleaner <- wow_clean[,-c(1,2,4,7:11, 17, 23:29)]
bio_NA <- dplyr::filter(wow_cleaner, is.na(biomasse))
wow_cleaner <- dplyr::filter(wow_cleaner, !is.na(biomasse))
bio_NA$biomasse = 0
wow_cleaner <- rbind(wow_cleaner, bio_NA) 
write.table(wow_cleaner, "zack_top_standard.txt")



test <- dplyr::filter(wow_clean, Order== "Hymenoptera")                        
test2 <- dplyr::filter(test, is.na(Family))

