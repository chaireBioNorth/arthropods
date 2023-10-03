#param <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/param_plus.txt")
param <- read.table("DATA/FINAL_DATA/param_plus.txt")
library(dplyr)
library(segmented)
library(MuMIn)
library(ggplot2)
library(nlme)
library(car)
library(xtable)
library(png)
library(grid)
options(scipen = 999)

seg_ampli <- function(param, type){
data <- param

#transformation données ----
# data$sdj196 <- scale(data$dj196, center = FALSE, scale = TRUE)

# Enlever données manquantes ----

data <- dplyr::filter(data, !is.na(Snowmelt_day))
# data <- dplyr::filter(data, Field_site == "Bylot"|Field_site == "Canning"|Field_site == "Colville"|Field_site == "Hochstetter"|Field_site == "Ikpikpuk"|Field_site == "Medusa"|Field_site == "Nome"|Field_site == "Southampton"|Field_site == "Utqiagvik"|Field_site == "Zackenberg")
# data <- dplyr::filter(data, !Field_site == "Herschel")
# data <- dplyr::filter(data, !Field_site == "Hochstetter")
#ajout colonne w ----

field_sites <- unique(data$Field_site)
m_data <- data.frame()

for (i in 1:length(field_sites)){
  fs <- field_sites[i]
  tab1<- data[which(data$Field_site==fs),]
  tab1$w <- 1/length(tab1$Period_Year)
  m_data <- rbind(m_data, tab1)
}
# Modèles >1 variable ----
m_nul<- lm(Peak_amplitude_raw~1, data=m_data, weights=w)

m_base <- lm(Peak_amplitude_raw~dj196, data = m_data, weights = w)
seg_base<- segmented(m_base, seg.Z = ~dj196, data=m_data, weights = w )

m_lsnow <- lm(Peak_amplitude_raw~dj196  +Snowmelt_day, data = m_data, weights = w)
seg_lsnow <- segmented(m_lsnow, seg.Z = ~dj196, data=m_data, weights = w )

m_lpr <- lm(Peak_amplitude_raw~dj196  + pr196, data = m_data, weights = w)
seg_lpr <- segmented(m_lpr, seg.Z = ~dj196, data=m_data, weights = w )

m_lrad <-lm(Peak_amplitude_raw~dj196 + rjuin5_juil15, data = m_data, weights = w)
seg_lrad <- segmented(m_lrad, seg.Z = ~dj196, data=m_data, weights = w )

m_snow <- lm(Peak_amplitude_raw~ Snowmelt_day, data = m_data, weights = w)

models_ampli <- list(mnul=m_nul, m_base=m_base, m_lrad=m_lrad, m_lpr=m_lpr, seg_base=seg_base, m_lsnow=m_lsnow, m_snow=m_snow, seg_lsnow=seg_lsnow, seg_lpr=seg_lpr, seg_lrad=seg_lrad)
mods_simple_ampli <- model.sel(m_nul, m_base,  m_lrad, m_lpr, seg_base, m_lsnow, m_snow, seg_lsnow, seg_lpr, seg_lrad)

write.csv(file="scripts/model_selection_ampli.csv", mods_simple_ampli)

#print(xtable(mods_simple_ampli[,c(11:15)]))
#print(xtable(mods_simple_ampli[,-c(5,8:10)]))
#réimporter data pour remettre toutes les données enlevées à cause de neige
# vif(m_lsnow)
data <- param
field_sites <- unique(data$Field_site)
m_data <- data.frame()

for (i in 1:length(field_sites)){
  fs <- field_sites[i]
  tab1<- data[which(data$Field_site==fs),]
  tab1$w <- 1/length(tab1$Period_Year)
  m_data <- rbind(m_data, tab1) }
# m_data <- dplyr::filter(m_data, !Field_site == "Herschel")
m_base <- lm(Peak_amplitude_raw~dj196, data = m_data, weights = w)
seg_base<- segmented(m_base, seg.Z = ~dj196, data=m_data, weights = w)

#model.sel(m_base, seg_base) # On sait pas à quoi ça sert
###



sum <- summary(seg_base)
conf <- confint(seg_base)
sl <- slope(seg_base)
int <- intercept(seg_base)
pt <- seg_base$psi[2]
# tab <- as.data.frame(slope(seg_base)$dj196)
# xtable(tab)
# figure ----
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
mean(prd_dj2$fit)
shape <- rep(c(21, 23, 24),7)
# gestion couleur ----
pal<-colorRamps::matlab.like(19)
pal[1]<-"#000000"
pal[2]<-"#5221B4"
pal[16]<-"#f24d06"
pal[17]<-"#e55709"
pal[18]<- "#b2154c"
pal[19]<-"#7a0e0e"
#icone<-readPNG("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/visuel/icone_peak_bio.png")
icone<-readPNG("visuel/icone_peak_bio.png")
i <- rasterGrob(icone, interpolate=TRUE)
ymax<- range(data$Peak_amplitude_raw, na.rm=T)[2]
ymin<-range(data$Peak_amplitude_raw, na.rm=T)[1]
y_loc <- ymax-((ymax-ymin)/3)
if( type == "sep") {
m_data<- m_data%>%
  mutate(Field_site = forcats::fct_reorder(Field_site, TMOY))
# png(filename="/Users/aureliechagnon-lafortune/Desktop/MAITRISE/memoire/figures/ampli_seg1.png",
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
    ,plot.margin = margin(0, 0, 0, 0.2, "cm")
  ) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        # legend.position="none",
        axis.line = element_line(color = 'black'), axis.ticks.length = unit(-0.15, "cm"),axis.text.y = element_text(margin=unit(c(0.35,0.25,0.35,0.25), "cm")))+
  geom_ribbon(data=prd_dj, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
  geom_smooth(data=prd_dj, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="solid")+
  geom_ribbon(data=prd_dj2, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
   geom_smooth(data=prd_dj2, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="dashed")+
  geom_point(data = m_data, aes(x = dj196, y = Peak_amplitude_raw,  fill=Field_site, shape = Field_site), alpha=0.55, size =2)+ scale_shape_manual(limits = param$Field_site, values=shape)+ scale_fill_manual(limits = param$Field_site, values=pal)+
  guides(col = guide_legend(ncol = 2))+theme(axis.title.y = element_text(margin = unit(c(0,0,0,0), "cm")))+
  labs( y= "Peak biomass (mg/trap/day)")+ theme(legend.title = element_blank(), legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm")) + annotate("text", x = 415, y=540, label= "b)", size=4)+
  annotation_custom(i, xmin=275, xmax=395, ymin=y_loc) #+ geom_vline(xintercept = pt, alpha=0.4, linetype="dashed")
g


} else {
  #graphs moyennes ----
  tabmoy <- data.frame()
  field_sites <- unique(data$Field_site)


  for (i in 1:length(field_sites)){
    Field_site <- field_sites[i]
    tab1<- data[which(data$Field_site==Field_site),]
    peak <- mean(tab1$Peak_amplitude_raw)
    peak_sd <- sd(tab1$Peak_amplitude_raw)
    dj196 <- mean(tab1$dj196)
    dj196_sd<- sd(tab1$dj196)
    TMOY<- unique(tab1$TMOY)
    tab2 <- data.frame(Field_site, peak, dj196, TMOY, peak_sd, dj196_sd)
    tabmoy <- rbind(tabmoy, tab2)
     }
  tabmoy<- tabmoy%>%
    mutate(Field_site = forcats::fct_reorder(Field_site, TMOY))
    g <-
      ggplot() +
      theme_bw() + theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_rect(color = "black")
        ,plot.margin = margin(0, 0, 0, 0.2, "cm")
      ) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            # legend.position="none",
            axis.line = element_line(color = 'black'), axis.ticks.length = unit(-0.15, "cm"),axis.text.y = element_text(margin=unit(c(0.35,0.25,0.35,0.25), "cm")))+
      geom_ribbon(data=prd_dj, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
      geom_smooth(data=prd_dj, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="solid")+
      geom_ribbon(data=prd_dj2, aes(x=dj196, ymin=lci, ymax=uci), fill="gray86")+
      geom_smooth(data=prd_dj2, aes(x=dj196, y= fit), stat = "identity", col= "gray35", linetype="dashed")+
geom_errorbar(data=tabmoy, aes(x=dj196,ymin=peak-peak_sd, ymax=peak+peak_sd), width=range(tabmoy$dj196)[2]/75, col="gray30")+
geom_errorbarh(data=tabmoy, aes(xmin=dj196-dj196_sd, xmax=dj196+dj196_sd, y=peak, height=range(tabmoy$peak)[2]/55), col="gray30")+
      geom_point(data = tabmoy, aes(x = dj196, y = peak, fill= Field_site, shape = Field_site), size =5)+ scale_shape_manual(values=shape)+ scale_fill_manual(values=pal)+
      guides(col = guide_legend(ncol = 2))+theme(axis.title.y = element_text(margin = unit(c(0,0,0,0), "cm")))+
      labs( y= "Peak biomass (mg/trap/day)")+
      theme(
        legend.title = element_blank(),
        legend.text = element_text( size = 8), legend.key.size = unit(0.5, "cm")) + annotate("text", x = 415, y=540, label= "b)", size=4)

    }

# print(g1)
# dev.off()
return(list(g=g, sum=sum, conf=conf, sl=sl, int=int, pt=pt, models_ampli=models_ampli))
}
