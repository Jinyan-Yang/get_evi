# TERN AusCover. [Year and version number of data product]. [Data product title]. Obtained from [server URL], made available by the AusCover facility (http://www.auscover.org.au) of the Terrestrial Ecosystem Research Network (TERN, http://www.tern.org.au). Accessed [Date accessed].

#This dataset has been developed by the Time Series Remote Sensing Team, CSIRO Marine and Atmospheric Research. The original data were supplied by the Land Processes Distributed Active Archive Center (LPDAAC), located at the U.S. Geological Survey (USGS) Earth Resources Observation and Science Center (EROS) http://lpdaac.usgs.gov.

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
    try(curl_download_withtest(file.path('downloads','modis',fn),urlZip))

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
  file.nm.vec <- sprintf("downloads/modis/MOD.%s.%03d.250m_evi.nc",out.df$year,out.df$doy)
  
  out.df$evi <- mapply(read.func,file.nm.vec)
  
  return(out.df)
}

# get.modis.nc.func(2016)

# download evi####
for(year in 2001:2016){
  get.modis.nc.func(year)
}

# get and save evi for coords####

# get ym
year.in <- 2001:2016
lat <- -33.610412
lon <- 150.73394
evi.ym.df <- get.evi.coords.ns.func(year.in,lat = lat ,lon=lon)
saveRDS(evi.ym.df,'ym20012016.rds')

# get qp
# -26.577250, 144.619028
year.in <- 2001:2016
lat <- -26.577250
lon <- 144.619028
evi.qp.df <- get.evi.coords.ns.func(year.in,lat = lat ,lon=lon)
saveRDS(evi.qp.df,'qp20012016.rds')

# ng
# -31.645194, 146.641889
year.in <- 2001:2016
lat <- -31.645194
lon <- 146.641889
evi.ng.df <- get.evi.coords.ns.func(year.in,lat = lat ,lon=lon)
saveRDS(evi.ng.df,'ng20012016.rds')

# DP
# -36.271964, 146.309585
year.in <- 2001:2016
lat <- -36.271964
lon <- 146.309585
evi.dp.df <- get.evi.coords.ns.func(year.in,lat = lat ,lon=lon)
saveRDS(evi.dp.df,'dp20012016.rds')



# make plot####
evi.ym.df <- readRDS('ym20012016.rds')
plot(evi~Date,data = evi.ym.df,type='b',pch=16,col='grey80',ylab='EVI')
plot(evi~Date,data = evi.ym.df[evi.ym.df$year %in% c('2013','2014'),],type='b',pch=16,col='grey',ylab='EVI')
huf.df <- read.csv('outPutHufken.csv')

pdf('evi_model.pdf',width = 8,height = 8*0.618)
par(mar=c(3,5,1,1))
par(bg = rgb(240/255,241/255,211/255))
plot(evi~Date,data = evi.ym.df[evi.ym.df$year %in% c('2013','2014'),],type='b',
     lwd=3,pch=16,col='grey80',
     xlim=c(as.Date('2013-1-1'),as.Date('2014-12-31')),
     xlab='',ylab='EVI')
par(new=T)
plot(huf.df$lai,type='s',ann=F,axes=F)

legend('topright',legend = c('MODIS','MODEL'),lty='solid',lwd=2,,col=c('grey','black'),bty='n')
dev.off()

par(mfrow=c(3,2))
for (i in 2011:2016) {
  plot(evi~Date,data = evi.ym.df[evi.ym.df$year %in% i,],type='b',pch=16,col='grey',ylab='EVI')
  legend('top',legend = i,bty='n')
}

# get watson evi####

watson.sites <- read.csv('watson_site.csv')

save.evi.sites.func <- function(row.watson){
  year.in <- 2001:2016
  
  tmp.df <- get.evi.coords.ns.func(year.in,lat = as.numeric(row.watson[2]),lon=as.numeric(row.watson[3]))
  saveRDS(tmp.df,sprintf('cache/evi.%s20012016.rds',row.watson[1]))
  
}

apply(watson.sites,1,save.evi.sites.func)