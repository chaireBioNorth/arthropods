clean <- function(data) {
  
  
  arthro_all <- data
arthro_all_safe <- arthro_all[-which(arthro_all$Arthropod_Order == "Lepidoptera" & arthro_all$Stage != "Larva" | arthro_all$Arthropod_Order == "lepi" | arthro_all$Arthropod_Order == "lepidoptera"),]
arthro_all_safe <- arthro_all_safe[-which(arthro_all_safe$Arthropod_Family == "Apidae"),]
arthro_all_safe <- arthro_all_safe[-which(arthro_all_safe$Arthropod_Family == "bomb"),] 
arthro_all_safe <- arthro_all_safe[-which(arthro_all_safe$Arthropod_Order == "Acari" | arthro_all_safe$Arthropod_Family == "Acari (fam N/A)"),]    
arthro_all_safe <- arthro_all_safe[-which(arthro_all_safe$Arthropod_Order == "Gastropoda" | arthro_all_safe$Arthropod_Order == "Phthiraptera"|  arthro_all_safe$Arthropod_Order == "Phtiraptera" | arthro_all_safe$Arthropod_Order == "Oligochaeta"| arthro_all_safe$Arthropod_Order == "Ixodida"| arthro_all_safe$Arthropod_Order == "Siphonaptera"| arthro_all_safe$Arthropod_Order == "Copepoda"| arthro_all_safe$Arthropod_Order == "Collembola"| arthro_all_safe$Arthropod_Order == "Other"| arthro_all_safe$Arthropod_Order == "na"| arthro_all_safe$Arthropod_Order == "Mesostigmata" | arthro_all_safe$Arthropod_Order == "Cladocera" | arthro_all_safe$Arthropod_Order == "Unknown"| arthro_all_safe$Arthropod_Order == "Megastomata"),]



return(arthro_all_safe)
}
