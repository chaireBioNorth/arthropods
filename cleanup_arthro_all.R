#### Importation des données et formatage de base ####

setwd("/Users/aureliechagnon-lafortune/Desktop/MAÎTRISE/DATA/raw")

# ...importer jeu de données Élise : "ARTRHO-ALL ----


arthro_all <- read.table("ARTHROPOD_ALL.txt")

# .....ménage de base ----

# sites 
unique(arthro_all$Field_site)

# ne garder que Bylot, Alert, Herschel et Southampton
arthro_all <- subset(arthro_all, Field_site %in% c("Alert", "Herschel", "Bylot", "Southampton"))

# enlever habitats superflus
arthro_all <- arthro_all[-which(arthro_all$Habitat =="Emergence" ), ]
arthro_all<- arthro_all[-which(arthro_all$Habitat =="snow fence" ), ]
arthro_all<- arthro_all[-which(arthro_all$Habitat =="Snow fence" ), ]
arthro_all<- arthro_all[-which(arthro_all$Habitat =="Mesic/snowfence" ), ]


unique(arthro_all$Habitat)


#ménage bylot
bylot <- dplyr::filter(arthro_all, Field_site == "Bylot")
unique(bylot$Trap_number)

#enlever trappes supplémentaires 2012
bylot <- filter(bylot, Trap_number != "11" & Trap_number != "12"&Trap_number != "13"&Trap_number != "14"&Trap_number != "15"&Trap_number != "16"&Trap_number != "17"&Trap_number != "18" & Area_Name != "Bylot_mesic_E")

# éliminer aires supplémentaires (ne garder que mesic_F et wet_D)
bylot<- filter(bylot, Area_Name != "Bylot_wet_A" & Area_Name != "Bylot_wet_B"& Area_Name != "Bylot_wet_C")
unique(bylot$Area_Name)

arthro_all2<- filter(arthro_all, Field_site != "Bylot")
arthro_all2<- rbind(arthro_all2, bylot)
arthro_all <- arthro_all2

#éliminer mesures négatives
arthro_all$Arthropod_Biomass <- ifelse(arthro_all$Arthropod_Biomass < 0, 0, arthro_all$Arthropod_Biomass)

# exporter fichier clean
write.table (arthro_all, "arthro_all_clean.txt")
