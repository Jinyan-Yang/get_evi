repro.modis.fun <- function(DF){
  library(rgdal)  
  sincrs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "
  s <- SpatialPoints(DF, proj4string=CRS(sincrs))
  
  lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
  
  coords.repro.tmp <- spTransform(s, lonlat)
  coords.repro.df <- as.data.frame(coords.repro.tmp)
  return(coords.repro.df)
}

coord2sinu.fun <- function(DF){
  library(rgdal)  
  
  lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
  s <- SpatialPoints(DF, proj4string=CRS(lonlat))
  sincrs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "

  coords.repro.tmp <- spTransform(s, sincrs)
  coords.repro.df <- as.data.frame(coords.repro.tmp)
  
  return(coords.repro.df)
}

# coords.repro.df2 <- repro.modis.fun(DF2)
# coords.repro.df <- repro.modis.fun(DF)
# range(coords.repro.df$x)
# range(coords.repro.df2$x)
# # spatial_ref: PROJCS["unnamed",GEOGCS["Unknown datum based upon the custom spheroid",
# DATUM["Not_specified_based_on_custom_spheroid",
#       SPHEROID["Custom spheroid",6371007.181,0]],
# PRIMEM["Greenwich",0],UNIT["degree",0.0174532925199433,
#                            AUTHORITY["EPSG","9122"]]],
# PROJECTION["Sinusoidal"],
# PARAMETER["longitude_of_center",0],
# PARAMETER["false_easting",0],
# PARAMETER["false_northing",0],
# UNIT["Meter",1],AXIS["Easting",EAST],AXIS["Northing",NORTH]]
# # GeoTransform: 13343406.236 463.3127165279165 0.0 -3335851.559 0.0 -463.3127165279167