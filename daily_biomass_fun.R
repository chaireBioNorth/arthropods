### Calculer biomasse/trappe/jour -----
daily_biomass <- function(data){
df <- data.frame()
# boucle sur les sites

sites = unique(data$Field_site)
for (a in 1: length(sites)) {
  
  site = sites[a]
  tab1 <- data[which(data$Field_site == site),]
  
  # boucle sur les années
  years = unique(tab1$Period_Year)
  
  for (b in 1: length(years)) {
    
    year = years[b]
    tab2 <- tab1[which(tab1$Period_Year == year),]
    
    # boucle sur les habitats
    
    habs = unique(tab2$Habitat)
    
    for (c in 1: length(habs)) {
      
      hab = habs[c]
      tab3 <- tab2[which(tab2$Habitat== hab),]
      
      # boucle sur les jours
      
      days = unique(tab3$Period_Day)
      
      for (d in 1:length(days)) {
        
        day = days[d]
        tab4 <- tab3[which(tab3$Period_Day == day),]
        
        # somme de la biomasse pour ce jour/milieu
        tab4$somme_biomasse <- sum(tab4$Arthropod_Biomass, na.rm=T)
        
        # division par le nombre  de trappes
        tab4$somme_trappe <- tab4$somme_biomasse/tab4$nb_of_trap
        
        # division par le nombre de jours de trappe
        tab4$mg_jour_trappe <- tab4$somme_trappe/tab4$trap_days
        
        df <- rbind(df, tab4)
      }}}}
return(df)
}
