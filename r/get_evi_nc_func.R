# library(curl)####
library(raster)
library(ncdf4)
library(gdalUtils)
library(R.utils)
library(lubridate)
#function to  downloaded file from url with test ####
curl_download_withtest <- function(filename, url,...){
  if(!file.exists(filename)){
    download.file(url, filename,"curl",quiet = FALSE,...)
  } else {
    message("Skipped ",filename, "; already exists.")
  }
}

get.modis.nc.func <- function(year){
  # http://remote-sensing.nci.org.au/u39/public/data/modis/lpdaac-mosaics-cmar/v1-hdf4/aust/MOD13Q1.005/
  # 2000.02.18/MOD13Q1.2000.049.aust.005.b02.250m_evi.hdf.gz
  
  urlBase <- 'http://remote-sensing.nci.org.au/u39/public/data/modis/lpdaac-mosaics-cmar/v2-nc4/aust/MOD13Q1.005/'
  
  yearDate <-  format(as.Date(sprintf("%s-01-01",year)),"%Y.%m.%d")
  date.seq <- seq(as.Date(sprintf("%s-01-01",year)),
                  as.Date(sprintf("%s-12-31",year)),by=16)
  doy  <-  yday(date.seq)
  
  for (i in seq_along(doy)){
    # url and file info
    urlZip <- paste0(urlBase,format(date.seq[i],"%Y.%m.%d"),
                     "/MOD13Q1.",year,".",sprintf("%03d.aust.005.", doy[i]),"enhanced_vegetation_index.nc")
   
    fn <- sprintf("MOD.%s.%03d.250m_evi.nc",as.character(year),doy[i])

    # download file
    try(curl_download_withtest(file.path('downloads',fn),urlZip))

  }

}

get.evi.coords.ns.func <- function(year.in,lat,lon){
  creat.date.func <- function(year){
    seq(as.Date(sprintf("%s-01-01",year)),
        as.Date(sprintf("%s-12-31",year)),by=16)
  }
  
  read.func <- function(fn){
    
    nc_data <- nc_open(fn)
    
    lon.vec <- ncvar_get(nc_data, "longitude")

    lat.vec <- ncvar_get(nc_data, "latitude")

    evi <- ncvar_get(nc_data, "evi")
    
    lat.index <- which.min(abs(lat.vec - lat))    
    lon.index <-  which.min(abs(lon.vec - lon)) 
    
    value.tar <- evi[lon.index,lat.index]
    rm(evi)
    return(value.tar)
  }
  
  date.ls <- list()
  for (i in seq_along(year.in)){
    date.ls[[i]] <- data.frame(Date = creat.date.func(year.in[i]))
  }
  
  out.df <- do.call(rbind,date.ls)
  out.df$year <- year(out.df$Date)
  out.df$doy <- yday(out.df$Date)
  out.df$lat <- lat
  out.df$lon <- lon
  
  # 
  file.nm.vec <- sprintf("downloads/MOD.%s.%03d.250m_evi.nc",out.df$year,out.df$doy)
  
  out.df$evi <- mapply(read.func,file.nm.vec)
  
  return(out.df)
}

# get.modis.nc.func(2016)

# download evi####
for(year in 2001:2016){
  get.modis.nc.func(year)
}

# get and save evi for coords####
year.in <- 2001:2016
lat <- -33.610412
lon <- 150.73394
evi.ym.df <- get.evi.coords.ns.func(year.in,lat = lat ,lon=lon)
saveRDS(evi.ym.df,'ym20012016.rds')
# make plot
evi.ym.df <- readRDS('ym20012016.rds')
plot(evi~Date,data = evi.ym.df,type='b',pch=16,col='grey80')
