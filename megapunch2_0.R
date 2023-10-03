#param <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/param_plus.txt")
setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/")
param <- read.table("DATA/FINAL_DATA/param_plus.txt")

load("memoire/figures/models_3fev2022.RData")

# Conf intervals for all top models
ls()
mdate

library(dplyr)
topmodels <- list(mdate$m_lpr, mdate$seg_lsnow, mdate$seg_lpr, ampli$models_ampli$seg_base, ampli$models_ampli$seg_lrad, ampli$models_ampli$seg_lpr, ampli$models_ampli$seg_lsnow, saison$models_sais$seg_lrad, saison$models_sais$seg_lsnow,saison$models_sais$seg_lpr)
params <- c("date", "date", "date", "date", "date", "date", "date","date", "date", "date")
models <- c("datem_lpr", "dateseg_lsnow", "dateseg_lpr", "ampli_seg_base", "ampli_seg_lrad", "ampli_seg_lpr","ampli_seg_lsnow", "sais_seg_lrad", "sais_seg_lsnow", "saison_seg_lpr")
confidence_intervals<-data.frame()
for (i in 1 :length(topmodels)){
  d <- topmodels[[i]] %>% confint() %>% as.data.frame()
  d$param <- params[i]
  d$model <- models[i]

  
  confidence_intervals <- rbind(confidence_intervals, d)
}



#setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures")
# setwd("memoire/figures")
library(ggplot2)
library(ggpubr)
options(scipen = 999)

#source('~/Desktop/MAITRISE/scripts/analyses/reg_ponderee_date_final.R')
source("scripts/analyses/reg_ponderee_date_final.R")
date <-reg_date(param, type="sep")


mdate <- date$models_date
mdate$m_lsnow %>% summary()
mdate$m_lpr %>% summary()
mdate$m_snow %>% summary()

#source('~/Desktop/MAITRISE/scripts/seg_ampli.R')
source("scripts/seg_ampli.R")
ampli <- seg_ampli(param, type="sep")
mampli <- ampli$models_ampli
mampli$seg_base %>% summary()

# source('~/Desktop/MAITRISE/scripts/seg_sais.R')
source("scripts/seg_sais.R")
saison <- seg_sais(param, type="sep")

setwd("memoire/figures/")

png(filename="megapunch2_5.png",
    units="in",
    width=7,
    height=10,
    pointsize=12,
    res=300)

mega_punch <- ggarrange(date$g, ampli$g, saison$g, ncol=1, nrow=3, 
                        common.legend = TRUE, legend = "right"
                       ,align = "v"
                        )
annotate_figure(mega_punch, bottom = text_grob("Cumulative thawing degree days between June 5 and July 15", hjust = 1,  x = 0.74 , size = 11))

dev.off()
# save.image(file="models_3fev2022.RData")
#load("/media/eliane/BlackBox/MAITRISE/memoire/figures/models_3fev2022.RData")

library(segmented)

mdate <- date$models_date
mampli <- ampli$models_ampli
msais <- saison$models_sais

list_seg <- list(mdate$seg_lsnow, mdate$seg_lpr, mdate$seg_base, 
                mdate$seg_lrad, mampli$seg_base, mampli$seg_lrad, 
                mampli$seg_lpr, mampli$seg_lsnow, msais$seg_base, 
                msais$seg_lrad, msais$seg_lsnow, msais$seg_lpr) 

for(i in 1:length(list_seg)){
  s <- slope(list_seg[[i]])
  print(summary(list_seg[[i]]))
  print(s)
}


mdate$m_lsnow %>% summary()
(0.25*1)+151.87

mdate$m_lsnow %>% confint()

mampli <- ampli$models_ampli
mampli$seg_base %>% summary()
summary(mampli$seg_base)
confint(mampli$seg_base)
slope(mampli$seg_base)
intercept(mampli$seg_base)
mampli$seg_base$psi[2]
mampli$seg_base %>% summary.segmented()
(106.171 + (1.96 * 21.301)) 
(106.171 - (1.96 * 21.301))
(25*1.74)


msais <- saison$models_sais
msais$seg_base %>% summary()
summary(msais$seg_base)
confint(msais$seg_base)
slope(msais$seg_base)
intercept(msais$seg_base)
msais$seg_base$psi[2]
msais$seg_base %>% summary.segmented()
(106.171 + (1.96 * 21.301)) 
(106.171 - (1.96 * 21.301))
(25*10.8)


# 
# 
# # points regroupés ----
# 
# g1 <-reg_date(param, type="ens")
# 
# g2 <- seg_ampli(param, type="ens")
# 
# g3<- seg_sais(param, type="ens")
# 
# png(filename="megapunch_group.png",
#     units="in",
#     width=7,
#     height=10,
#     pointsize=12,
#     res=300)
# 
# mega_punch <- ggarrange(g1, g2, g3, ncol=1, nrow=3, common.legend = TRUE, legend="right", align = "v")
# annotate_figure(mega_punch, bottom = text_grob("Cumulative thaw degree days between June 5 and July 15", hjust = 1,  x = 0.65 , size = 11))
# 
# dev.off()
