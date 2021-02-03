
library(lubridate)
library(ncdf4)
get.met.coords.ns.func <- function(year.in,met.var,lat,lon){
  
  date.start.month <- seq(as.Date(sprintf("%s-01-01",year.in)),length=12,by="months")
  date.end.month <- seq(as.Date(sprintf("%s-02-01",year.in)),length=12,by="months")-1
  
  read.func <- function(fn){
    
    nc_data <- nc_open(fn)
    nc_data$var$rain_day
    lon.vec <- ncvar_get(nc_data, "longitude")
    
    lat.vec <- ncvar_get(nc_data, "latitude")
    
    met.df <- ncvar_get(nc_data, met.var)
    
    lat.index <- c()
    lon.index <- c()
    for (i in seq_along(lat)) {
      lat.index[i] <- which.min(abs(lat.vec - lat[i]))    
      lon.index[i] <-  which.min(abs(lon.vec - lon[i])) 
    }
    
    
    
    value.tar <- met.df[lon.index,lat.index]
    rm(met.df)
    return(value.tar)
  }
  
  # date.ls <- list()
  # for (i in seq_along(year.in)){
  #   date.ls[[i]] <- data.frame(Date = creat.date.func(year.in[i]))
  # }
  
  # out.df <- do.call(rbind,date.ls)
  out.df <- data.frame(Date = seq(as.Date('2001-1-1'),
                                  as.Date('2016-12-31'),
                                  by='month'))
  out.df$year <- year(out.df$Date)
  out.df$mon <- month(out.df$Date)
  out.df$lat <- lat
  out.df$lon <- lon
  
  # rain.2001.01.nc
  file.nm.vec <- sprintf("cache/rainfall.rds",
                         out.df$year,out.df$mon)
  
  out.df$rain <- mapply(read.func,file.nm.vec)
  
  return(out.df)
}

# get map by coord

get.map.coord.func <- function(lat,lon){
  rain.m <- readRDS('cache/rainfall.rds')
  rain.coord.ls <- readRDS('cache/rain.coord.rds')
  
  lon.vec <- rain.coord.ls[['lon']]
  
  lat.vec <- rain.coord.ls[['lat']]
  
  lat.index <- c()
  lon.index <- c()
  map.vec <- c()
  for (i in seq_along(lat)) {
    lat.index[i] <- which.min(abs(lat.vec - lat[i]))    
    lon.index[i] <-  which.min(abs(lon.vec - lon[i])) 
    
    map.vec[i] <- rain.m[lon.index[i],lat.index[i]]
  }
  return(map.vec)
}

get.map.coord.func(c(-30,-32),c(150,147))

