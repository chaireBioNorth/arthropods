# param <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/param_plus.txt")
param <- read.table("/media/eliane/BlackBox/MAITRISE/DATA/FINAL_DATA/param_plus.txt")
library(ggplot2)
#library(svglite)
#install.packages("svglite")
library(dplyr)

# stats d'oiseaux pour scénarios 2.0 ----
#ASDN ----
# asdn<- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/FINAL_DATA/ASDN_Bird_nests.txt", header = TRUE, sep = "\t", dec = ".", blank.lines.skip=TRUE)
asdn <- read.table("/media/eliane/BlackBox/MAITRISE/DATA/FINAL_DATA/ASDN_Bird_nests.txt", header = TRUE, sep = "\t", dec = ".", blank.lines.skip=TRUE)
asdn <- dplyr::filter(asdn, !is.na(estHatch_julian))
asdn$Site %>% unique()

get_dates <- function(site, type, n_spp=3){
# site <- dplyr::filter(asdn, Site == "barr")
# n_spp=3
  #all species, 90% des nids ----
  # mean(site$estHatch_julian, na.rm=T)
  # 
  # tab_hd <- data.frame()
  # years <- unique(site$Year)
  # data <- NULL
  # 
  # for (a in 1:length(years)){
  #   year <- years[a]
  #   tab1 <- site[which(site$Year==year),]
  #   high <-quantile(tab1$estHatch_julian, probs = 0.95 , na.rm=T)
  #   low<-quantile(tab1$estHatch_julian, probs = 0.05 , na.rm=T)
  #   tab1<- dplyr::filter(tab1, estHatch_julian<=high & estHatch_julian>=low)
  #   mhd <- mean(tab1$estHatch_julian, na.rm=T)
  #   lhd <- range(tab1$estHatch_julian, na.rm=T)[1]
  #   uhd <- range(tab1$estHatch_julian, na.rm=T)[2]
  #   rhd <- uhd-lhd
  #   n <- length(tab1$Nest_ID)
  #   tab<- data.frame( year,  mhd, uhd, lhd, rhd,n)
  #   tab_hd <- rbind(tab_hd, tab)
  #   tab_hd <- do.call(data.frame, lapply(tab_hd, function(x) replace(x, is.infinite(x),NA)))
  # }
  # mean_mhd_hd <- mean(tab_hd$mhd,  na.rm=TRUE)
  # # mean_ann_range <- mean(tab_hd$rhd)
  # mean_lhd <- mean(tab_hd$lhd,  na.rm=TRUE)
  # mean_uhd <- mean(tab_hd$uhd,  na.rm=TRUE)
  # sp <- "all"
  # dfall <- data.frame(sp=sp, mean_mhd_hd = mean_mhd_hd, mean_lhd= mean_lhd, mean_uhd=mean_uhd, n=n)

  summ <- site %>%
    group_by(Species) %>%
    summarise(no_rows = length(Species))
  top <- dplyr::slice_max(summ, no_rows, n=n_spp)
  spp <- top$Species
  # sp1 <- dplyr::filter(cold, Species=="reph")
  # sp2 <- dplyr::filter(cold, Species=="dunl")
  # sp3 <- dplyr::filter(cold, Species=="pesa")

  allsp <- NULL
  for(i in 1:length(spp)){

  spx <- dplyr::filter(site, Species==spp[[i]])
  sp <- unique(spx$Species)
  #sp1 - 90% des nids ----
  mean(spx$estHatch_julian, na.rm=T)
  high <- quantile(spx$estHatch_julian, probs = 0.95 , na.rm=T)
  low <- quantile(spx$estHatch_julian, probs = 0.05 , na.rm=T)
  spx <- dplyr::filter(spx, estHatch_julian<=high & estHatch_julian>=low)
  mhd <- mean(spx$estHatch_julian, na.rm=T)
  lhd <- range(spx$estHatch_julian, na.rm=T)[1]
  uhd <- range(spx$estHatch_julian, na.rm=T)[2]
  tabsp <- c(sp, mhd, lhd, uhd)
  allsp <- rbind(allsp, tabsp)
  }
  return(allsp)
}

barr <- dplyr::filter(asdn, Site == "barr")
(dates_barr <- get_dates(site=barr, type="cold"))

ikpi <- dplyr::filter(asdn, Site == "ikpi")
(dates_ikpi<- get_dates(site=ikpi, type="cold"))

bylo <- dplyr::filter(asdn, Site == "bylo")
(dates_bylo <- get_dates(site=bylo, type="cold"))

nome <- dplyr::filter(asdn, Site == "nome")
(dates_nome <- get_dates(site=nome, type="warm"))

chur <- dplyr::filter(asdn, Site == "chur")
(dates_chur <- get_dates(site=chur, type="warm"))

cakr <- dplyr::filter(asdn, Site == "cakr")
(dates_cakr <- get_dates(site=cakr, type="warm"))
