library(h5)
library(sp)
library(raster)
library(rgdal)


# Raw data directory
setwd("/Users/aureliechagnon-lafortune/Desktop/ERA-interim/")

# Subdirectory creation
dir.create("tif", showWarnings = FALSE)

# List of Matlab files
fls <- list.files(path = "mat/", pattern = "\\.mat$", full.names = TRUE)

# Variables names
varnames <- c("clt", "pr", "rsds", "snd", "tas", "tasmax", "tasmin", "tsl1")

# Final raster grid
grid <- raster(
  xmn        = -180,
  xmx        =  180,
  ymn        =  -90,
  ymx        =   90,
  resolution = 0.75,
  crs        = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
)

# Loop on Matlab files
#commencer à snd 2014-12-14    
for (i in 37:length(fls)) {

i
  # Connexion to Matlab file i
  file <- h5file(fls[i])

  # Extract coordinates
  lon  <- as.vector(file["/out_all/lon/data"][])
  lat  <- as.vector(file["/out_all/lat/data"][])

  # Extract dates
  jour <- file["/out_all/time_vectors/data"][]
  jour <- as.Date(paste(jour[1, ], jour[2, ], jour[3, ], sep = "-"))


  # Loop on variables
  
  
  for (k in 1:length(varnames)) {
  #k=1
 
  
    varname <- varnames[k]

    # Extract values
    var <- file[paste0("/out_all/", varname, "/data")][]

    # Loop on dates
    
    
  for (j in 1:length(jour)) {
  
      # Print progress
      cat("\r", paste(varname, jour[j], "   "))

      # Data frame with coordinates and variable values
      xy <- data.frame(lon, lat, value = as.vector(var[ , , j]))

      # Report values on the raster
      pos <- cellFromXY(grid, xy[ , 1:2])
      ras <- grid ; ras[][pos] <- xy[ , 3]

      # Set raster name
      names(ras) <- paste0(varname, "_", gsub("-", "", jour[j]))

      # Define study area (> 45°N)
      area <- data.frame(lon = c(-180, 180), lat = c(45, 90))
      area <- SpatialPoints(coords = area)
      proj4string(area) <- projection(ras)
      area <- as(extent(area), "SpatialPolygons")
      
      # Clip raster
      ras <- mask(ras, area)
      ras <- crop(ras, area)

      # Export raster
      writeRaster(ras, paste0("tif/", names(ras), ".tif"), format = "GTiff")
    }
  }
}
