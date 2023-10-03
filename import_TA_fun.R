
import_TA <- function(data){ 
  
TA <- data

#enlever lignes vides
TA <- dplyr::filter(TA, !is.na(nr))

# ..........individus mesurés --> comptes d'individus ####
# chaque ligne est un individu mesuré, le but est de combiner tous les individus d'un même groupe

# Boucle sur les annees pas nécessaire puisqu'on travaille directement sur la date

mega <- data.frame()

# Boucle sur la date

days <- unique(TA$Period_date)

for (i in 1:length(days)) {
  # i=2
  day <- days[i]

  
  tab1 <- TA[which(TA$Period_date == day), ]
  
  # Boucle sur les habitats
  habs <- unique(tab1$Habitat)
  
  for (j in 1:length(habs)) {
    #j=1
    hab <- habs[j]
    tab2 <- tab1[which(tab1$Habitat == hab), ]
    
    # boucle sur les ordres
    ords <- unique(as.character(tab2$Arthropod_Order))
    
    for (k in 1:length(ords)) {
      # k=1
      ord <- ords[k]
      tab3 <- tab2[which(tab2$Arthropod_Order == ord), ]
      
      # boucle sur les familles
      families <- unique(as.character(tab3$Arthropod_Family))
      
      for (l in 1:length(families))  {
        # l=1
        family <- families[l]
        tab4 <- tab3[which(tab3$Arthropod_Family == family), ]
        
        abondance_totale <- length(tab4$nr)
        
        
        
        
        
        # Biomasse totale TOTALE
        biomasse_totale <- sum(na.omit(tab4$Arthropod_Biomass))
        
        
        dat <- data.frame(day = day, ord=ord, family=family, biomasse_totale=biomasse_totale, habs=hab, abondance_totale = abondance_totale, n_of_traps = unique(tab4$n_of_traps) )
        
        mega <- rbind(mega, dat)
        
      }
    }
  }
}

# ..........gestion date ####
ta_date <- mega

ta_date$day<- as.character(ta_date$day)

ta_date$day<- as.Date(ta_date$day, format = "%d-%m-%y")

ta_date$Period_Year <- format(ta_date$day, "%Y")

ref4 <- paste(ta_date$Period_Year, "-01-01", sep = "")
ref4 <- as.Date(ref4)

ta_date$julian <- (ta_date$day - ref4)+1
ta_date$Period_Day <-as.integer(ta_date$julian)

str(ta_date)

#construire fichier compatible avec les autres
taimyr <- ta_date
taimyr$Field_site <-as.factor( "Taimyr")
taimyr$Habitat = taimyr$habs

taimyr$Arthropod_Family <- taimyr$family
taimyr$Arthropod_Order <- taimyr$ord

taimyr$Arthropod_Count <- taimyr$abondance_totale

taimyr$Arthropod_Biomass <- taimyr$biomasse_totale
taimyr$year <- taimyr$Period_Year
taimyr$nb_of_trap <- taimyr$n_of_traps

taimyr$ID <- 1:length(taimyr$day)


# résultat : tableau avec biomasse totale pour chaque milieu, chaque groupe + nb_of_traps + trap_days

# ..........ajout des trap_days ----
dfTA <- data.frame()

years<- unique(taimyr$Period_Year)
for (i in 1: length(years)) {
  
  y <- years[i]
  tab2<- taimyr[which(taimyr$Period_Year == y), ]
  
  #boucle sur l'habitat
  habitat <-unique(tab2$Habitat)
  
  for (k in 1:length(habitat)){
    
    hab <- habitat[k]
    tab3 <-  tab2[which(tab2$Habitat == hab), ]
    
    #boucle sur les jours 
    #créer banque de jours
    days <- sort(unique(as.numeric(as.character(tab3$Period_Day))))
    length(days)
    
    
    for (j in 2:length(days)) {
      
      day <- days[j]
      tab4 <- tab3[which(tab3$Period_Day == day), ]
      tab4$trap_days <- days[j]-days[j-1]
      
      
      dfTA<-rbind(dfTA, tab4)
    }}}

dfTA <- dfTA[,-c(1:7,9, 17)]
dfTA$Period_Year <- as.integer(dfTA$Period_Year)


source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
dfTA<-daily_biomass(dfTA)
write.table(dfTA, file = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/dfTA_pre_conv.txt")
source('~/Desktop/MAITRISE/scripts/CAL_conv_fun.R')
dfTA<-CAL_conv(dfTA)

return(dfTA)

}

