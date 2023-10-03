library(sp) 
library(raster)
library(rgdal)
library(tidyr)



station1 <- data.frame(sta = "Bylot", x = -79.96760, y = 73.15782)
station2 <- data.frame(sta = "Alert", x = -62.467921, y = 82.507821)
station3 <- data.frame(sta="Herschel", x= -138.900, y= 69.567)
station4 <-data.frame(sta="Southampton", x= -81.66640, y =63.98366 )
station5 <-data.frame(sta="Churchill", x= -94.06667, y =58.75000 )
station6 <- data.frame(sta="Zackenberg", x= -20.56667, y =74.47 )
station7 <- data.frame(sta="Taimyr", x= 80.50000, y =73.33333 )
station8 <- data.frame(sta="Igloolik", x=-81.557208, y=69.395803)
station9 <- data.frame(sta="Akimiski", x= -81.333333, y =53)
station10 <- data.frame(sta="Nome", x=-164.9613, y=64.4445)
station11 <- data.frame(sta="Krusenstern", x = -163.4955, y=67.1142)
station12 <- data.frame(sta="Colville", x = -150.6764, y=70.4369)
station13 <- data.frame(sta="Prudhoe", x = -148.4499, y=70.1977)
station14 <- data.frame(sta="Canning", x=-145.8506, y =70.1179)
station15 <- data.frame(sta="Ikpikpuk", x=-154.7309, y=70.5525)
station16 <- data.frame(sta="Utqiagvik", x= -156.7600, y=71.3015)
station17 <- data.frame (sta= "Mackenzie", x=-134.8878, y=69.3710)
station18 <- data.frame (sta="Chipp", x=-155.30, y= 70.68)
station19 <- data.frame(sta="Hochstetter", x=-19.70, y=75.15)

 stations<- rbind(station1, station2, station3, station4, station5, station6, station7, station8, station9,station10, station11, station12, station13, station14, station15, station16, station17, station18, station19)


#pour avoir une référence de projection : 
ERA <- raster("/Users/aureliechagnon-lafortune/Desktop/Maitrise/ERA-interim/tif/tsl1_20170731.tif")

#extraction des fichiers de neige
setwd("/Users/aureliechagnon-lafortune/Desktop/Snowmelt_timing_maps_1504/ALL_2001-2015/snow")
variable <- "Snowmelt_Timing_North_America_"
years <- 2001:2017
fls_all <- list.files(
  path    = getwd(),
  pattern = paste0(paste0(variable, years), collapse = "|")
)
modis_all <- stack(fls_all)



setwd("/Users/aureliechagnon-lafortune/Desktop/Snowmelt_timing_maps_1504/cell_AL")

variable <- "Snowmelt_Timing_h17v00_"
years <- 2000:2018
fls_AL <- list.files(
  path    = getwd(),
  pattern = paste0(paste0(variable, years), collapse = "|")
)

modis_AL <- stack(fls_AL)

# setwd("/Users/aureliechagnon-lafortune/Desktop/Snowmelt_timing_maps_1504/cell_BY/snow/")
# variable <- "Snowmelt_Timing_h15v01_"
# years <- 2016:2018
# fls_BY <- list.files(
#   path    = getwd(),
#   pattern = paste0(paste0(variable, years), collapse = "|")
# )
# 
# modis_BY <- stack(fls_BY)
# 
# setwd("/Users/aureliechagnon-lafortune/Desktop/Snowmelt_timing_maps_1504/cell_IG/snow/")
# 
# 
# variable <- "Snowmelt_Timing_h15v02_"
# years <- 2016:2018
# fls_IG <- list.files(
#   path    = getwd(),
#   pattern = paste0(paste0(variable, years), collapse = "|")
# )
# modis_IG <- stack(fls_IG)

setwd("/Users/aureliechagnon-lafortune/Desktop/Snowmelt_timing_maps_1504/cell_TA")
variable <- "Snowmelt_Timing_h20v01_"
years <- 2000:2018
fls_TA <- list.files(
  path    = getwd(),
  pattern = paste0(paste0(variable, years), collapse = "|")
)
modis_TA <- stack(fls_TA)

#définir projection des stations
sta <- SpatialPointsDataFrame(coords = stations[ , c("x", "y")], data = data.frame(stations[ , "sta"]))
proj4string(sta) <- proj4string(ERA)
#rgdal::writeOGR(obj=sta, dsn = "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/qgis/stations", layer = "stations2023", driver = "ESRI Shapefile")
proj4string(sta)
sta <- spTransform(sta, CRS(proj4string(modis_all)))


# extraction modis_all ----
neige <- raster::extract(modis_all, sta)
tab_neige_all <- as.data.frame(neige)
tab_neige_all$Field_site <- stations$sta
tab_neige <- tab_neige_all[-c(2,7),]#retirer AL, TA 
names(tab_neige) <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "Field_site")
tab_neige2 <- gather(data = tab_neige, key="Period_Year", value="Snowmelt_day", c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))

#extraction AL+ZA ----
neigeAL <- raster::extract(modis_AL, sta)
tab_neige_AL <- as.data.frame(neigeAL)
tab_neige_AL$Field_site <- stations$sta
tab_neige_AL <- tab_neige_AL[c(2),] # ne garder que les lignes avec valeurs

names(tab_neige_AL) <- c("2000","2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Field_site")
tab_neige_AL2 <- gather(data = tab_neige_AL, key="Period_Year", value="Snowmelt_day", c("2000","2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))


# #extraction BY suppl ----
# 
# neigeBY <- raster::extract(modis_BY, sta)
# tab_neige_BY <- as.data.frame(neigeBY)
# tab_neige_BY$Field_site <- stations$sta
# tab_neige_BY <- tab_neige_BY[1,]
# names(tab_neige_BY) <- c( "2016", "2017", "2018", "Field_site")
# tab_neige_BY2 <- gather(data = tab_neige_BY, key="Period_Year", value="Snowmelt_day", c("2016", "2017", "2018"))
# 
# #extraction IG suppl ----
# 
# neigeIG <- raster::extract(modis_IG, sta)
# tab_neige_IG <- as.data.frame(neigeIG)
# tab_neige_IG$Field_site <- stations$sta
# tab_neige_IG <- tab_neige_IG[8,]
# names(tab_neige_IG) <- c( "2016", "2017", "2018", "Field_site")
# tab_neige_IG2 <- gather(data = tab_neige_IG, key="Period_Year", value="Snowmelt_day", c("2016", "2017", "2018"))


#extraction TA ----

neigeTA <- raster::extract(modis_TA, sta)
tab_neige_TA <- as.data.frame(neigeTA)
tab_neige_TA$Field_site <- stations$sta
tab_neige_TA <- tab_neige_TA[7,]

names(tab_neige_TA) <- c("2000","2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "Field_site")
tab_neige_TA2 <- gather(data = tab_neige_TA, key="Period_Year", value="Snowmelt_day", c("2000","2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"))

#fusion de toutes les tables
NEIGE <- as.data.frame(rbind(tab_neige2, tab_neige_AL2, 
                             #tab_neige_BY2, tab_neige_IG2, 
                             tab_neige_TA2))
# tab_neige2 <- rbind(tab_neige2, tab_AL)
# tab_neige2<- rbind(tab_neige2, tab_BY)
# tab_neige2 <- rbind(tab_neige2, tab_IG)

setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires")
write.table (NEIGE, file="date_fonte.txt")

