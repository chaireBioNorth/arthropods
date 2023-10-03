#param <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/param_plus.txt")
param <- read.table("DATA/FINAL_DATA/param_plus.txt")
library(dplyr)
library(segmented)
library(ggplot2)
#library(esquisse)
library(MuMIn)
library(png)
library(grid)

options(scipen = 999)

seg_sais <- function(param, type){

data <- param

# Enlever données manquantes ----

data <- dplyr::filter(data, !is.na(Snowmelt_day))
data<- dplyr::filter(data, !is.na(somme_sais_raw))
# data<- dplyr::filter(data, !Field_site == "Herschel")
# data <- dplyr::filter(data, Field_site == "Bylot"|Field_site == "Canning"|Field_site == "Colville"|Field_site == "Hochstetter"|Field_site == "Ikpikpuk"|Field_site == "Medusa"|Field_site == "Nome"|Field_site == "Southampton"|Field_site == "Utqiagvik"|Field_site == "Zackenberg")



#ajout colonne w ----

field_sites <- unique(data$Field_site)
m_data <- data.frame()

for (i in 1:length(field_sites)){
  fs <- field_sites[i]
  tab1<- data[which(data$Field_site==fs),]
  tab1$w <- 1/length(tab1$Period_Year)
  m_data <- rbind(m_data, tab1)
}
# m_data <- m_data[-c(28,29),]


# Modèles >1 variable ----
m_nul <- lm(somme_sais_raw~1, data=m_data, weights=w)

m_base <- lm(somme_sais_raw~dj196, data = m_data, weights = w)
seg_base<- segmented(m_base, seg.Z = ~dj196, data = m_data, weights = w)

# m_quad<-lm(somme_sais_raw~dj196+I(dj196^2), data = m_data, weights = w)
#m_qsnow<-lm(somme_sais_raw~dj196+I(dj196^2)+Snowmelt_day, data = m_data, weights = w)
m_lsnow <- lm(somme_sais_raw ~ dj196 + Snowmelt_day, data = m_data, weights = w)
seg_lsnow <- segmented(m_lsnow, seg.Z = ~dj196, data=m_data, weights = w )

m_lpr <- lm(somme_sais_raw ~ dj196  + pr196, data = m_data, weights = w)
#m_qpr <- lm(somme_sais_raw~dj196 +I(dj196^2) + pr196, data = m_data, weights = w)
seg_lpr <- segmented(m_lpr, seg.Z = ~dj196, data=m_data, weights = w )

m_lrad <-lm(somme_sais_raw ~ dj196 + rjuin5_juil15, data = m_data, weights = w)
#m_qrad <-lm(somme_sais_raw~dj196 +I(dj196^2)+rMoy_juin_juillet, data = m_data, weights = w)
seg_lrad <- segmented(m_lrad, seg.Z = ~ dj196, data=m_data, weights = w )

m_snow <- lm(somme_sais_raw ~ Snowmelt_day, data = m_data, weights = w)

models_sais <- list(mnul=m_nul, m_base=m_base, m_lrad=m_lrad, m_lpr=m_lpr, seg_base=seg_base, m_lsnow=m_lsnow, m_snow=m_snow, seg_lsnow=seg_lsnow, seg_lpr=seg_lpr, seg_lrad=seg_lrad)
mods_simple_sais <- model.sel(m_nul, m_base, m_lrad, m_lpr, seg_base, m_lsnow, m_snow, seg_lsnow, seg_lpr, seg_lrad)

write.csv(file="scripts/model_selection_sais.csv", mods_simple_sais)
# print(xtable(mods_simple_sais[,c(10:14)]))
# print(xtable(mods_simple_sais[,-c(5,8:10)]))

#réimporter data pour remettre toutes les données enlevées à cause de neige

data <- param
data<- dplyr::filter(data, !is.na(somme_sais_raw))
# data<- dplyr::filter(data, !Field_site == "Herschel")
field_sites <- unique(data$Field_site)
m_data <- data.frame()

for (i in 1:length(field_sites)){
  fs <- field_sites[i]
  tab1<- data[which(data$Field_site==fs),]
  tab1$w <- 1/length(tab1$Period_Year)
  m_data <- rbind(m_data, tab1) }

m_base <- lm(somme_sais_raw~dj196, data = m_data, weights = w)
seg_base<- segmented(m_base, seg.Z = ~dj196)
pt <- seg_base$psi[2]
sum <- summary(seg_base)
conf <- confint(seg_base)
int <- intercept(seg_base)
sl <- slope(seg_base)
# tab <- as.data.frame(slope(seg_base)$dj196)
# xtable(tab)


prd_dj <- data.frame(dj196=seq(from=range(m_data$dj196)[1], to=pt, length.out=200 ))
err <- predict(seg_base, newdata=prd_dj, re.form=NA, se.fit=T, interval = "confidence")
t <- as.data.frame(err$fit)

prd_dj$lci <- t$lwr
prd_dj$fit <- t$fit
prd_dj$uci <- t$upr

prd_dj2 <- data.frame(dj196=seq(from=pt, to=range(m_data$dj196)[2], length.out=200 ))
err <- predict(seg_base, newdata=prd_dj2, re.form=NA, se.fit=T, interval = "confidence")
t <- as.data.frame(err$fit)

prd_dj2$lci <- t$lwr
prd_dj2$fit <- t$fit
prd_dj2$uci <- t$upr





# gestion couleur ----
pal<-colorRamps::matlab.like(19)
pal[1]<-"#000000"
pal[2]<-"#5221B4"
pal[16]<-"#f24d06"
pal[17]<-"#e55709"
pal[18]<- "#b2154c"
pal[19]<-"#7a0e0e"

shape <- rep(c(21, 23, 24),7)

#icone<-readPNG("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/visuel/icone_seas_bio.png")
icone<-readPNG("visuel/icone_seas_bio.png")
i <- rasterGrob(icone, interpolate=TRUE)

ymax<- range(data$somme_sais_raw, na.rm=T)[2]
ymin<-range(data$somme_sais_raw, na.rm=T)[1]
y_loc <- ymax-((ymax-ymin)/3)

if( type == "sep") {
data<-param
  data<- data%>%
  mutate(Field_site = forcats::fct_reorder(Field_site, TMOY))


# png(filename="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures/saison_seg1.png",
# units="in",
# width=7,
# height=4,
# pointsize=12,
# res=300)
g <-
  ggplot() +
  theme_bw() + theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(color = "black")
    ,plot.margin = margin(0, 0, 0.5, 0.2, "cm")
  )  +theme(axis.title.x = element_blank(),legend.position="none", axis.line = element_line(color = 'black'), axis.ticks.length = unit(-0.15, "cm"), axis.text.y = element_text(margin=unit(c(0.35,0.25,0.35,0.25), "cm")), axis.text.x = element_text(margin=unit(c(-0.30,-0.3,-0.3,-0.3), "cm")))+
  geom_ribbon(data=prd_dj, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
  geom_smooth(data=prd_dj, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="solid")+
   geom_ribbon(data=prd_dj2, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
  geom_smooth(data=prd_dj2, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="dashed")+
  geom_point(data = data, aes(x = dj196, y = somme_sais_raw, fill= Field_site, shape=Field_site), alpha=0.55, size =2)+scale_shape_manual(limits = param$Field_site, values=shape)+ scale_fill_manual(limits = param$Field_site, values=pal, name="Study site")+
  guides(col = guide_legend(ncol = 2))+theme(axis.title.y = element_text(margin = unit(c(0,0,0,0), "cm")))+
  labs(x= "Cumulative thaw degree days on July 15th", y= "Seasonal biomass (mg)")+ theme(legend.title = element_blank(),legend.text = element_text( size = 8), legend.key.size = unit(0.5, "cm"))+ annotate("text", x = 415, y=4400, label= "c)", size=4)+ annotation_custom(i, xmin=275, xmax=395, ymin=y_loc)

g
# print(g1)
# dev.off()
}else{
  data<- param
  tabmoy <- data.frame()
  field_sites <- unique(data$Field_site)


  for (i in 1:length(field_sites)){
    Field_site <- field_sites[i]
    tab1<- data[which(data$Field_site==Field_site),]
    sais <- mean(tab1$somme_sais_raw)
    sais_sd <- sd(tab1$somme_sais_raw)
    dj196 <- mean(tab1$dj196)
    dj196_sd<- sd(tab1$dj196)
    TMOY<- unique(tab1$TMOY)
    tab2 <- data.frame(Field_site, sais, dj196, TMOY, sais_sd, dj196_sd)
    tabmoy <- rbind(tabmoy, tab2)
  }
#graph moyennes ----

tabmoy<- tabmoy%>%
  mutate(Field_site = forcats::fct_reorder(Field_site, TMOY))


g<-
  ggplot() +
  theme_bw() + theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(color = "black")
    ,plot.margin = margin(0, 0, 0.5, 0.2, "cm")
  )  +theme(axis.title.x = element_blank(),legend.position="none", axis.line = element_line(color = 'black'), axis.ticks.length = unit(-0.15, "cm"), axis.text.y = element_text(margin=unit(c(0.35,0.25,0.35,0.25), "cm")), axis.text.x = element_text(margin=unit(c(-0.30,-0.3,-0.3,-0.3), "cm")))+
  geom_ribbon(data=prd_dj, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
  geom_smooth(data=prd_dj, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="solid")+
  geom_ribbon(data=prd_dj2, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
  geom_smooth(data=prd_dj2, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="dashed")+
  geom_errorbar(data=tabmoy, aes(x=dj196,ymin=sais-sais_sd, ymax=sais+sais_sd), width=range(tabmoy$dj196)[2]/75, col="gray30")+
  geom_errorbarh(data=tabmoy, aes(xmin=dj196-dj196_sd, xmax=dj196+dj196_sd, y=sais, height=range(tabmoy$sais)[2]/65), col="gray30")+
  geom_point(data = tabmoy, aes(x = dj196, y = sais, fill= Field_site, shape=Field_site), size =5)+scale_shape_manual(values=shape)+ scale_fill_manual(values=pal)+
  guides(col = guide_legend(ncol = 2))+theme(axis.title.y = element_text(margin = unit(c(0,0,0,0), "cm")))+
  labs(x= "Cumulative thaw degree days on July 15th", y= "Seasonal biomass (mg)")+
  theme(
    legend.title = element_blank(),
    legend.text = element_text( size = 8), legend.key.size = unit(0.5, "cm"))+ annotate("text", x = 415, y=4400, label= "c)", size=4)

}
# print(g2)

return(list(g=g, sum=sum, conf=conf, sl=sl, int=int, pt=pt, models_sais=models_sais))
}
