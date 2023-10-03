
#param <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/param_plus.txt")
param <- read.table("./DATA/FINAL_DATA/param_plus.txt")
# setwd("memoire/figures")
library(dplyr)
library(lme4)
library(ggplot2)
# library(outliers)
library(MuMIn)
library(xtable)
library(knitr)
library(car)
library(png)
library(grid)
options(scipen = 999)
reg_date <- function(param,type){
  library(segmented)
data <- param
data$Peak_date_raw[which(data$Field_site == "Zackenberg")] <- NA
data$Peak_date_raw[which(data$Field_site == "Chipp")] <- NA
data$Peak_date_raw[which(data$Field_site == "Hochstetter")] <- NA
# data <- dplyr::filter(data, Field_site == "Bylot"|Field_site == "Canning"|Field_site == "Colville"|Field_site == "Hochstetter"|Field_site == "Ikpikpuk"|Field_site == "Medusa"|Field_site == "Nome"|Field_site == "Southampton"|Field_site == "Utqiagvik"|Field_site == "Zackenberg")
# data$Peak_date_raw[which(data$Field_site == "Taimyr")] <- NA
#data$Peak_date_raw[which(data$Field_site == "Akimiski")] <- NA
# data <- dplyr::filter(data, Field_site != "Zackenberg")
# data <- dplyr::filter(data, Field_site != "Chipp")
min(data$Peak_date_raw, na.rm=T)
max(data$Peak_date_raw, na.rm=T)
slice_min(data, Peak_date_raw, n=10)
slice_max(data, Peak_date_raw, n=10)
#ajout colonne w ----
data <- dplyr::filter(data, !is.na(Snowmelt_day))

field_sites <- unique(data$Field_site)
m_data <- data.frame()

for (i in 1:length(field_sites)){
  fs <- field_sites[i]
  tab1<- data[which(data$Field_site==fs),]
  tab1$w <- 1/length(tab1$Period_Year)
  m_data <- rbind(m_data, tab1)
}


# Sélection de modèles ----
## Modèles pondérés

data <- m_data
str(m_data)

  m_nul <- lm(Peak_date_raw~1, data=data, weights=w)

  m_base <- lm(Peak_date_raw~dj196, data = data, weights = w)
  seg_base<- segmented(m_base, seg.Z = ~dj196,data = data, weights = w)

  m_lrad <-lm(Peak_date_raw~dj196 + rjuin5_juil15, data = data, weights = w)
  seg_lrad <- segmented(m_lrad, seg.Z = ~dj196, data=data, weights = w )

  # m_lprsn<- lm(Peak_date_raw~dj196 + Snowmelt_day+ pr196, data = data, weights = w)
  # seg_lprsn <- segmented(m_lprsn, seg.Z = ~dj196, data=data, weights = w )

  m_lsnow <-lm(Peak_date_raw~dj196  +Snowmelt_day, data = data, weights = w)
  seg_lsnow <- segmented(m_lsnow, seg.Z = ~dj196, data=data, weights = w )

  m_lpr <- lm(Peak_date_raw~dj196  + pr196, data = data, weights = w)
  seg_lpr <- segmented(m_lpr, seg.Z = ~dj196, data=data, weights = w )

  m_snow <-lm(Peak_date_raw~Snowmelt_day, data = data, weights = w)
  summary(m_lsnow)
  summary(m_lpr)
  #summary(m_lprsn)

  models_date <- list(mnul=m_nul, m_base=m_base, m_lrad=m_lrad, m_lpr=m_lpr, seg_base=seg_base, m_lsnow=m_lsnow, m_snow=m_snow, seg_lsnow=seg_lsnow, seg_lpr=seg_lpr, seg_lrad=seg_lrad)
  mods_simple <- model.sel(m_nul,m_base, m_lrad,m_lsnow, m_lpr, m_snow, seg_base, seg_lsnow, seg_lpr, seg_lrad)

  write.csv(file="scripts/model_selection_date.csv", mods_simple)
  # print(xtable(mods_simple[,c(11:15)]))
  # print(xtable(mods_simple[,-c(6, 8:10)]))
  sum <- summary(m_lsnow)
  conf <- confint(m_lsnow)
  # d4 <- get.models(mods_simple, subset = delta < 2)
 # m_avg<-  model.avg(d4)
# plot(m_lsnow)
  #  test<-summary(m_lprsn)
  # xtable(test$coefmat)
# confint(m_avg, full= T)
#  vif(m_lprsn)
#  vif(m_quad)
 # print(xtable(mods_simple[,c(7:11)]))

#
#
#
# # Figures  ----


# comm. png pour figure toute seule pour présentations ----
# png(filename="date_dj.png",
# units="in",
# width=12.6,
# height=5.5,
# pointsize=12,
# res=300)
#graph de dj196 (snow stable)----
  #créer "newdata"
prd_dj <- data.frame(dj196=seq(from=range(data$dj196)[1], to=range(data$dj196)[2], length.out=200 ))
prd_dj$Snowmelt_day <-mean(data$Snowmelt_day)
#prd_dj$pr196 <-mean(data$pr196)
newdata <-prd_dj

# pred <- data.frame(
#   model = sapply(d4, predict, newdata = newdata),
#   # averaged.subset = predict(m_avg, newdata, full = FALSE),
#   averaged.full = predict(m_avg, newdata, full = TRUE)
# )
err <- predict(m_lsnow, newdata=prd_dj, re.form=NA, se.fit=T, interval = "confidence")
t <- as.data.frame(err$fit)

prd_dj$lci <- t$lwr
prd_dj$fit <- t$fit
prd_dj$uci <- t$upr



limits_x <- c(15, 430)
breaks_x <- seq(15, 430, by = 50)
labels_x <- seq(15, 430, by = 50)

#graph de dj196 (snow et pr stable)----
# range(m_data_date$Peak_date_raw)

limits_y <- c(155, 215)
breaks_y <- seq(155, 215, by = 15)

m_data<- m_data%>%
  mutate(Field_site = forcats::fct_reorder(Field_site, TMOY))
# gestion couleur ----
pal<-colorRamps::matlab.like(19)
pal[1]<-"#000000"
pal[2]<-"#5221B4"
pal[16]<-"#f24d06"
pal[17]<-"#e55709"
pal[18]<- "#b2154c"
pal[19]<-"#7a0e0e"
shape <- rep(c(21, 23, 24),7)

#icone<-readPNG("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/visuel/icone_peak_date.png")
icone<-readPNG("visuel/icone_peak_date.png")
i <- rasterGrob(icone, interpolate=TRUE)
ymax<- range(data$Peak_date_raw, na.rm=T)[2]
ymin<-range(data$Peak_date_raw, na.rm=T)[1]
y_loc <- ymax-((ymax-ymin)/3)
if( type == "sep") {
g <-
  ggplot(prd_dj, aes(x=dj196, y=fit)) +
  theme_bw() + theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(color = "black")
    ,plot.margin = margin(0.1, 0, 0, 0.2, "cm")
  ) +
  theme(
    axis.title.x = element_blank(), axis.text.x = element_blank(),
    #legend.position="none",
    axis.line = element_line(color = 'black'), axis.ticks.length = unit(-0.15, "cm"),axis.text.y = element_text(margin=unit(c(0.35,0.25,0.35,0.25), "cm")))+
  theme(axis.title.y = element_text(margin = unit(c(0,0,0,0), "cm")))+
  scale_y_continuous(name="Peak date (DOY)", limits=limits_y, breaks = breaks_y) +
  geom_line()+
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity",col= "gray35", linetype="solid") +
  geom_point(data = m_data, aes(x = dj196, y = Peak_date_raw, fill= Field_site, shape=Field_site), size =2, alpha=0.55)+
  # guides(col = guide_legend(nrow = 8))+
  scale_shape_manual(limits = param$Field_site, values=shape)+ scale_fill_manual(limits = param$Field_site, values=pal) + labs(fill="Study sites", shape="Study sites")+
  theme(
    legend.title = element_text(color="black", size=12),
    legend.text = element_text(size = 10), legend.key.size = unit(0.75, "cm")) + annotate("text", x = 415, y=214, label= "a)", size=4)+
  annotation_custom(i, xmin=275, xmax=395, ymin=y_loc)
g
} else {
  tabmoy <- data.frame()
  field_sites <- unique(data$Field_site)


  for (i in 1:length(field_sites)){
    Field_site <- field_sites[i]
    tab1<- data[which(data$Field_site==Field_site),]
    date <- mean(tab1$Peak_date_raw)
    date_sd <- sd(tab1$Peak_date_raw)
    dj196 <- mean(tab1$dj196)
    dj196_sd<- sd(tab1$dj196)
    TMOY<- unique(tab1$TMOY)
    tab2 <- data.frame(Field_site, date, dj196, TMOY, date_sd, dj196_sd)
    tabmoy <- rbind(tabmoy, tab2)
  }
  #graph moyennes ----
  tabmoy<- tabmoy%>%
    mutate(Field_site = forcats::fct_reorder(Field_site, TMOY))
  g <-
    ggplot() +
    theme_bw() + theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,panel.border = element_rect(color = "black")
      ,plot.margin = margin(0.1, 0, 0, 0.2, "cm")
    ) +
    theme(
      axis.title.x = element_blank(), axis.text.x = element_blank(),
      #legend.position="none",
      axis.line = element_line(color = 'black'), axis.ticks.length = unit(-0.15, "cm"),axis.text.y = element_text(margin=unit(c(0.35,0.25,0.35,0.25), "cm")))+
    theme(axis.title.y = element_text(margin = unit(c(0,0,0,0), "cm")))+
    scale_y_continuous(name="Peak date", limits=limits_y, breaks = breaks_y) +
    geom_ribbon(data=prd_dj, aes(x=dj196, ymin=lci, ymax=uci), fill="gray92")+
    geom_smooth(data=prd_dj, aes(x=dj196, y= fit), stat = "identity", col= "gray43")+
    geom_errorbar(data=tabmoy, aes(x=dj196,ymin=date-date_sd, ymax=date+date_sd), width=range(tabmoy$dj196)[2]/75, col="gray30")+
    geom_errorbarh(data=tabmoy, aes(xmin=dj196-dj196_sd, xmax=dj196+dj196_sd, y=date, height=range(tabmoy$date)[2]/65), col="gray30")+
    geom_point(data = tabmoy, aes(x = dj196, y = date, fill= Field_site, shape=Field_site), size =5)+scale_shape_manual(values=shape)+ scale_fill_manual(values=pal)+
    # guides(col = guide_legend(nrow = 8))+
    scale_shape_manual(values=shape)+ scale_fill_manual(values=pal)+ theme(
      # legend.title = element_text("Study sites"),
      legend.text = element_text(size = 10), legend.key.size = unit(0.75, "cm")) + annotate("text", x = 415, y=214, label= "a)", size=4)

  g

}
return(list(g=g, conf=conf, sum=sum, models_date=models_date))
}

# range(m_data$Peak_date_raw)
# limits_x <- c(15, 430)
# breaks_x <- seq(15, 430, by = 50)
# labels_x <- seq(15, 430, by = 50)
# limits_y <- c(160, 215)
# breaks_y <- seq(160, 215, by = 20)
#
# m_data<- m_data%>%
#   mutate(Field_site = forcats::fct_reorder(Field_site, Moy_juin_juillet))
#
# pal<-colorRamps::matlab.like(18)
# shape <- rep(c(21,24),9)
#
# g1 <-
#   ggplot(prd_dj, aes(x=dj196, y=fit)) +
#   theme_bw() + theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_rect(color = "black")
#   ) +
#   scale_x_continuous(name="Cumulative thaw degree days on July 15th", limits=limits_x, breaks = breaks_x, labels = labels_x) +
#   scale_y_continuous(name="Peak date", limits=limits_y, breaks = breaks_y) +
#   geom_line()+
#   geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
#   geom_point(data = m_data, aes(x = dj196, y = Peak_date_raw, fill= Field_site, shape=Field_site), size =3)+
#   guides(col = guide_legend(nrow = 8))+scale_shape_manual(values=shape)+ scale_fill_manual(values=pal)
# print(g1)
# dev.off()

# png(filename="date_snow.png",
#     units="in",
#     width=7,
#     height=4,
#     pointsize=12,
#     res=300)
# #graph de Snowmelt_day (dj196 stable) ----
# prd_dj <- data.frame(Snowmelt_day=seq(from=range(m_data$Snowmelt_day)[1], to=range(m_data$Snowmelt_day)[2], length.out=200 ))
# prd_dj$dj196<-mean(m_data$dj196)
# prd_dj$pr196<-mean(m_data$pr196)
# newdata <-prd_dj
#
# pred <- data.frame(
#   model = sapply(d4, predict, newdata = newdata),
#   # averaged.subset = predict(m_avg, newdata, full = FALSE),
#   averaged.full = predict(m_avg, newdata, full = TRUE)
# )
#
# prd_dj$fit <- pred$averaged.full
# pred.se <- predict(m_avg, prd_dj, se.fit = TRUE)
# prd_dj$lci <- prd_dj$fit - 1.96 * pred.se$se.fit
# prd_dj$uci <- prd_dj$fit + 1.96 * pred.se$se.fit
#
# range(m_data$Snowmelt_day)
# limits_x <- c(140, 215)
# breaks_x <- seq(140, 215, by = 20)
# labels_x <- seq(140, 215, by = 20)
# limits_y <- c(160, 215)
# breaks_y <- seq(160, 215, by = 20)
# labels_y <- seq(160, 215, by = 20)
#
#
#
# g2<-
#   ggplot(prd_dj, aes(x=Snowmelt_day, y=fit)) +
#   theme_bw() + theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_rect(color = "black")
#   ) +
#   scale_x_continuous(name="Snowmelt day", limits=limits_x, breaks = breaks_x, labels = labels_x) +
#   scale_y_continuous(name="Peak date", limits=limits_y, breaks = breaks_y, labels = labels_y) +
#   # geom_line()+
#   geom_abline(intercept = 20, slope=1, linetype="dashed", color="darkgrey")+
#   annotate("text", x=193, y=207, label= "1:1 ratio")+
#   geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity", linetype="dashed") +
#   geom_point(data = m_data, aes(x = Snowmelt_day, y = Peak_date_raw, fill= Field_site), pch=21, size=3)+scale_shape_manual(values=shape)+ scale_fill_manual(values=pal) + theme(
#     legend.title = element_blank(),
#     legend.text = element_text( size = 8), legend.key.size = unit(0.5, "cm"))
# print(g2)
# dev.off()
#
#
#
# png(filename="date_pr196.png",
#     units="in",
#     width=7,
#     height=4,
#     pointsize=12,
#     res=300)
#graph de pr196 (dj196 stable) ----
# prd_dj <- data.frame(pr196=seq(from=range(m_data$pr196)[1], to=range(m_data$pr196)[2], length.out=200 ))
# prd_dj$dj196<-mean(m_data$dj196)
# prd_dj$Snowmelt_day<- mean(m_data$Snowmelt_day, na.rm=T)
#
# newdata <- prd_dj
# pred <- data.frame(
#   model = sapply(d4, predict, newdata = newdata),
#   # averaged.subset = predict(m_avg, newdata, full = FALSE),
#   averaged.full = predict(m_avg, newdata, full = TRUE)
# )
#
# prd_dj$fit <- pred$averaged.full
# pred.se <- predict(m_avg, prd_dj, se.fit = TRUE)
# prd_dj$lci <- prd_dj$fit - 1.96 * pred.se$se.fit
# prd_dj$uci <- prd_dj$fit + 1.96 * pred.se$se.fit
#  range(m_data$pr196)
# limits_x <- c(5, 105)
# breaks_x <- seq(5, 105, by = 20)
# labels_x <- seq(5, 105, by = 20)
# limits_y <- c(160, 215)
# breaks_y <- seq(160, 215, by = 20)
# labels_y <- seq(160, 215, by = 20)
#
#
#
#
# g2<-
#   ggplot(prd_dj, aes(x=pr196, y=fit)) +
#   theme_bw() + theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_rect(color = "black")
#   ) +
#   scale_x_continuous(name="Cumulative precipitations on July 15th (mm)", limits=limits_x, breaks = breaks_x, labels = labels_x) +
#   scale_y_continuous(name="Peak date", limits=limits_y, breaks = breaks_y, labels = labels_y) +
#   # geom_line()+
#    geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
#   geom_point(data = m_data, aes(x = pr196, y = Peak_date_raw, fill= Field_site), pch=21, size=3)+ scale_fill_manual(values=pal) + theme(
#     legend.title = element_blank(),
#     legend.text = element_text( size = 8), legend.key.size = unit(0.5, "cm"))
# print(g2)
# dev.off()
#
#
# print(c("VIF", vif(m_lsnow)))
#
