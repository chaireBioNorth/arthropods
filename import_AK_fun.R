import_AK <- function(data){
  akimiski <- data
# ménage
akimiski<- akimiski[,-c(1,3,5:13, 15:17, 20:24, 28,29, 32:39)]
# ..........ajout des trap_days ----
# boucle sur les années 
dfAK <- data.frame()
years<- unique(akimiski$Period_Year)
for (i in 1: length(years)) {
  
  y <- years[i]
  tab2<- akimiski[which(akimiski$Period_Year == y), ]
  
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
      
      # ........... ajout nb_of_traps ----
      n_trap   <- length(unique(as.character(tab4$Trap_number)))
      
      tab4$nb_of_trap <- n_trap
      dfAK<-rbind(dfAK, tab4)
      
    }}}
source('~/Desktop/MAITRISE/scripts/daily_biomass_fun.R')
dfAK<-daily_biomass(dfAK)

return(dfAK)
}
