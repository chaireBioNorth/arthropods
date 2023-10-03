CAL_conv <- function(data){
library(ggplot2)
  library(ggExtra)
CAL<- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/data_CAL.txt")

#modèle simple tous habitats
mCAL <-lm(blanc~0+jaune, CAL)

# summary(mCAL)
#  plot(mCAL)

# #régression segmentée ----
# library(segmented)
# 
# segmented.mod <- segmented(mCAL, seg.Z = ~jaune)
# # summary(segmented.mod)
# # print(segmented.mod)
# data<- test
# data<- ZA
# data<- TA
#data <-HO
data2<- data

data2$jaune <- data2$mg_jour_trappe

prd <- predict(mCAL, data2)
data2$pred <-prd
data$mg_jour_trappe <- prd

return (data)
}


#graph----
# library(ggplot2)
# # 
# CAL$Period_day <- as.factor(CAL$Period_day)
# 
# prd <- data.frame(jaune=seq(from=range(CAL$jaune)[1], to=range(CAL$jaune)[2], length.out=200 ))
# err <- predict(mCAL, newdata=prd, re.form=NA, se.fit=T, interval = "confidence")
# t <- as.data.frame(err$fit)
# 
# prd$lci <- t$lwr
# prd$fit <- t$fit
# prd$uci <- t$upr
# const <- max(prd$fit)
# 
# png(filename="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures/CAL.png",
#         units="in",
#         width=7,
#         height=4,
#         pointsize=12,
#         res=300)
# #
# p <- ggplot(data=prd, aes(x=jaune, y=fit))  + geom_smooth(data=prd, aes(ymin = lci, ymax = uci), stat = "identity") + geom_point(data=CAL, aes(x=jaune, y=blanc, fill=Period_day), size=3, shape=21) + scale_fill_brewer(palette="YlGnBu", type = div)+ labs(x="Biomass in yellow cup traps (mg/day for each trap)", y="Biomass in white rectangle traps (mg/day for each trap)", fill="DOY")  + theme_bw() + theme(plot.background = element_blank() ,panel.grid.major = element_blank() ,panel.grid.minor = element_blank() ,panel.border = element_rect(color = "black"))
# print(p)
# dev.off()
# 
# #graph valeurs prédites Taimyr ----
# library(ggExtra)
# TA <- read.table(file = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/dfTA_pre_conv.txt")
# #rouler le reste du script ici #
# png(filename="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures/CAL_TA.png",
#     units="in",
#     width=7,
#     height=4,
#     pointsize=12,
#     res=300)
# p2 <- ggplot(data=data2, aes(x=mg_jour_trappe, y=pred)) + geom_point(shape=1)+geom_vline(xintercept = range(CAL$jaune)[1], col="darkblue")+geom_vline(xintercept = range(CAL$jaune)[2], col="darkblue") +annotate("text", x = 24, y=90, label= "83 %", size=4)+ labs(x= "Biomass of arthropods captured in traps (mg/trap/day)", y = "Converted biomass used in analyses (mg/trap/day)")
# 
# print(p2)
# dev.off()
# included <- dplyr::filter(TA, mg_jour_trappe>=range(CAL$jaune)[1] & mg_jour_trappe<=range(CAL$jaune)[2])
# incl_perc <- length(included$Period_Day)/length(TA$Period_Day) *100
#82.86183


# 
# #graph valeurs prédites Zack ----
# 
# ZA <- read.table(file = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/dfZA_pre_conv.txt")
# #rouler le reste du script ici #
# 
# png(filename="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures/CAL_ZA.png",
#     units="in",
#     width=7,
#     height=4,
#     pointsize=12,
#     res=300)
# p3 <- ggplot(data=data2, aes(x=mg_jour_trappe, y=pred)) + geom_point(shape=1)+geom_vline(xintercept = range(CAL$jaune)[1], col="darkblue") +geom_vline(xintercept = range(CAL$jaune)[2], col="darkblue") +annotate("text", x = 24, y=90, label= "47 %", size=4) + labs(x= "Biomass of arthropods captured in traps (mg/trap/day)", y = "Converted biomass used in analyses (mg/trap/day)")
# 
# 
# print(p3)
# dev.off()
# included <- dplyr::filter(ZA, mg_jour_trappe>=range(CAL$jaune)[1] & mg_jour_trappe<=range(CAL$jaune)[2])
# incl_perc <- length(included$Period_Day)/length(ZA$Period_Day) *100
# #46.37519


#graph valeurs prédites Hoch ----



# HO <- read.table(file = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/dfHO_pre_conv.txt")
# #rouler le reste du script ici #
# 
# png(filename="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures/CAL_HO.png",
#     units="in",
#     width=7,
#     height=4,
#     pointsize=12,
#     res=300)
# p4 <- ggplot(data=data2, aes(x=mg_jour_trappe, y=pred)) + geom_point(shape=1)+geom_vline(xintercept = range(CAL$jaune)[1], col="darkblue") +geom_vline(xintercept = range(CAL$jaune)[2], col="darkblue") +annotate("text", x = 24, y=90, label= "97 %", size=4) + labs(x= "Biomass of arthropods captured in traps (mg/trap/day)", y = "Converted biomass used in analyses (mg/trap/day)")
# 
# 
# print(p4)
# dev.off()
# included <- dplyr::filter(HO, mg_jour_trappe>=range(CAL$jaune)[1] & mg_jour_trappe<=range(CAL$jaune)[2])
# incl_perc <- length(included$Period_Day)/length(HO$Period_Day) *100
#97.33

# 
# 
# png(filename="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures/CAL_ZA.png",
#     units="in",
#     width=7,
#     height=4,
#     pointsize=12,
#     res=300)
# p3 <- ggplot(data=data2, aes(x=mg_jour_trappe, y=pred)) + geom_point(shape=20)+geom_vline(xintercept = range(CAL$jaune)[1], col="darkblue") +geom_vline(xintercept = range(CAL$jaune)[2], col="darkblue") +annotate("text", x = 24, y=90, label= " %", size=4) + labs(x= "Measured mg/trap/day", y = "Predicted mg/trap/day")
# ho<-ggMarginal(p3, type = "histogram", fill="lightsteelblue", margins = "x")
# 
# 
# dev.off()
# 
# #46.37519
