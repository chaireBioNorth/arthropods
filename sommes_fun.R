
somme_hab <- function(data){
## VERSION AVEC SOMMES GROSSIÈRES par habitat ####
data <-dplyr::filter(data, !is.na(trap_days))
tout <- data

mega <- data.frame()

# Boucle pour les sites

sites <- unique(tout$Field_site)
for (i in 1:length(sites)) { 
  
  site <- sites[i]
  tab1 <- tout[which(tout$Field_site == site), ]
  
  years <- unique(tab1$Period_Year)
  
  # Boucle sur les annees
  for (j in 1:length(years)) {
    
    year <- years[j]
    
    tab2 <- tab1[which(tab1$Period_Year == year), ]
    
    days <- sort(unique(tab2$Period_Day))
    
    # Boucle sur les jours
    for (k in 1:length(days)) {
      
      day <- days[k]
      
      tab3 <- tab2[which(tab2$Period_Day == day), ]
      
      # boucle sur les habitats
      habitat <- unique(tab3$Habitat)
      for (l in 1: length(habitat)) {
        
        hab <- habitat[l]
        tab4 <- tab3[which(tab3$Habitat == hab), ]
        tab5 <-tab4[1, -c(5,7:17,23:25)]
        td <- tab5$trap_days
        tab6 <- tab5[rep(row.names(tab5), td), ]
        pday <- unique(tab6$Period_Day)
        tab6$Period_Day = c((pday-td+1):pday) 
        mega <- rbind(mega, tab6)
     #ajout de doublon sert à équilibrer s'il y a une alternance de jours d'échantillonnages, et que ça soit vraiment une moyenne des deux habitats (et non une alternance des valeurs des 2 habitats)
        
      }}}}
return(mega)
}
## VERSION AVEC MOYENNES GROSSIÈRES par site/jour ####
somme_site_jour <- function(data) {
mega2 <- data.frame()
mega <- data
# Boucle pour les sites
# i=1
# j=1
# k=1


sites <- unique(mega$Field_site)
for (i in 1:length(sites)) { 
  
  site <- sites[i]
  tab1 <- mega[which(mega$Field_site == site), ]
  
  years <- unique(tab1$Period_Year)
  
  # Boucle sur les annees
  for (j in 1:length(years)) {
    
    year <- years[j]
    
    tab2 <- tab1[which(tab1$Period_Year == year), ]
    
    days <- sort(unique(tab2$Period_Day))
    
    # Boucle sur les jours
    for (k in 1:length(days)) {
      
      day <- days[k]
      
      tab3 <- tab2[which(tab2$Period_Day == day), ]
      
      #additionner les 2 habitats
      mg_trappe_jour<- mean(tab3$mg_jour_trappe)
      
      tab4 <- tab3[1,-c(4,8,9)]
      tab4$mg_jour_trappe <- mg_trappe_jour
      
      mega2 <- rbind(mega2, tab4)
      
    }}}
return(mega2)
}
