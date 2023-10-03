library(sp) 
library(raster)
library(rgdal)



### Import stations coordinates

#stations <- read.delim(...)

 station1 <- data.frame(sta = "bylot", x = -80.00000, y = 73.13333)
 station2 <- data.frame(sta = "alert", x = -62.33889, y = 82.50139)
 station3 <- data.frame(sta="herschel", x= -138.900, y= 69.567)
 station4 <-data.frame(sta="southampton", x= -81.66640, y =63.98366 )
 station5 <-data.frame(sta="churchill", x= -94.06667, y =58.75000 )
 station6 <- data.frame(sta="zack", x= -20.56667, y =74.47 )
 station7 <- data.frame(sta="taimyr", x= 80.50000, y =73.33333 )
 station8 <- data.frame(sta="igloolik", x=-81.7987, y=69.3489)
 station9 <- data.frame(sta="akimiski", x= -81.333333, y =53)
 station10 <- data.frame(sta="nome", x=-164.9613, y=64.4445)
 station11 <- data.frame(sta="krusenstern", x = -163.4955, y=67.1142)
 station12 <- data.frame(sta="colville", x = -150.6764, y=70.4369)
 station13 <- data.frame(sta="prudhoe", x = -148.4499, y=70.1977)
 station14 <- data.frame(sta="canning", x=-145.8506, y =70.1179)
 station15 <- data.frame(sta="ikpikpuk", x=-154.7309, y=70.5525)
 station16 <- data.frame(sta="utqiagvik", x= -156.7600, y=71.3015)
 station17 <- data.frame (sta= "mackenzie", x=-134.8878, y=69.3710)
 station18 <- data.frame (sta="Chipp", x=-155.30, y= 70.68)
station19 <- data.frame(sta="Hochstetter", x=-19.70, y=75.15)
stations <- station19

 stations[ , "sta"] <- as.character(stations[ , "sta"])


# Rasters directory
setwd("/Users/aureliechagnon-lafortune/Desktop/ERA-interim/tif")

# Temporal length
years <- 2011:2017

# Variables names
 variables <- c("clt", "pr", "rsds", "snd", "tas", "tasmax", "tasmin", "tsl1")


# Loop on variables
for (j in 1:length(variables)) {

  variable <- variables[j]

  # List of rasters for jth variable
  fls <- list.files(
    path    = getwd(),
    pattern = paste0(paste0(variable, "_", years), collapse = "|")
  )

  # Sub-table initialisation
  dat <- as.data.frame(matrix(nrow = length(fls), ncol = nrow(stations) + 1))
  colnames(dat) <- c("date", paste0(variable, "_", stations[ , "sta"]))

  # Loop on raster
  for (i in 1:length(fls)) {

    # Raster connexion
    ras <- raster(fls[i])
    # ERA <- ras
    # Print progress
    cat(paste0("Extract ", names(ras), "    \r"))

    # Extract values for stations
    dat[i, -1] <- raster::extract(ras, stations[ , -1])

    # Extract date
    dte <- gsub(paste0(variable, "_"), "", names(ras))
    dat[i, "date"] <- as.character(as.Date(dte, format = "%Y%m%d"))
  }

  # Results storage
  if (j == 1) {
    tab <- dat
  } else {
    tab <- merge(tab, dat, by = "date", all = TRUE)
  }
}

# Export climate table
# setwd("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires")
 # asdn_west <- read.table("/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/table_climate_asdn_west.txt", sep = "\t", header = T)
 
 
 # test <- dplyr::full_join(asdn_west, tab)
 # write.table(test, "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/table_climate_asdn_west.txt", sep = "\t", row.names = FALSE)
 write.table(tab, "/Users/aureliechagnon-lafortune/Desktop/MAITRISE/DATA/tables_intermediaires/table_climate_hoch.txt", sep = "\t", row.names = F)




