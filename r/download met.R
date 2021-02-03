# The [Data product title] data were obtained through TERN AusCover (http://www.auscover.org.au). TERN is Australia’s land-based ecosystem observatory delivering data streams to enable environmental research and management (TERN, http://www.tern.org.au). TERN is a part of Australia’s National Collaborative Research Infrastructure Strategy (NCRIS, https://www.education.gov.au/national-collaborative-research-infrastructure-strategy-ncris).


# library(curl)####	
# library(raster)

library(ncdf4)
# library(gdalUtils)
# library(R.utils)
# library(lubridate)
#function to  downloaded file from url with test ####
curl_download_withtest <- function(filename, url,...){
  if(!file.exists(filename)){
    download.file(url, filename,"curl",quiet = FALSE,...)
  } else {
    message("Skipped ",filename, "; already exists.")
  }
}


# ####
get.met.nc.func <- function(year,met.var){
  # http://rs-data1-mel.csiro.au/thredds/fileServer/bawap/
  # tmin/day/
  # 2018/bom-tmin_day-20180101-20180131.nc
  
  urlBase <- 'http://rs-data1-mel.csiro.au/thredds/fileServer/bawap/'
  
  date.start.month <- seq(as.Date(sprintf("%s-01-01",year)),length=12,by="months")
  date.end.month <- seq(as.Date(sprintf("%s-02-01",year)),length=12,by="months")-1
  
  for (i in 1:12){
    # url and file info
    s.date <- format(date.start.month[i],"%Y%m%d")
    e.date <- format(date.end.month[i],"%Y%m%d")
    urlZip <- paste0(urlBase,met.var,'/day/',year,
                     '/bom-',met.var,'_day-',s.date,'-',e.date,".nc")
    
    fn <- sprintf("%s.%s.%02d.nc",met.var,as.character(year),i)
    
    # download file
    # print(urlZip)
    if(!dir.exists(file.path('downloads/',met.var))){
      dir.create(file.path('downloads/',met.var))
    }
    try(curl_download_withtest(file.path('downloads/',met.var,fn),urlZip))
  }
}

for (year.in in 2001:2016) {
  # get.met.nc.func(year.in,'tmax')
  # get.met.nc.func(year.in,'tmin')
  # get.met.nc.func(year.in,'rain')
  # get.met.nc.func(year.in,'rad')
  get.met.nc.func(year.in,'vph15')
}
get.met.nc.func(2005,'vph15')
get.met.nc.func(2006,'vph15')
# get.met.nc.func(2015,'rain')




# http://rs-data1-mel.csiro.au/thredds/fileServer/bawap/rad/day/1993/bom-rad_day-19930101-19930131.nc
# get.tmax.nc.func <- function(year){
# 
#   # http://rs-data1-mel.csiro.au/thredds/fileServer/bawap/tmax/day/
#   # 2017/bom-tmax_day-20170101-20170131.nc
#   urlBase <- 'http://rs-data1-mel.csiro.au/thredds/fileServer/bawap/tmax/day/'
#   
#   yearDate <-  format(as.Date(sprintf("%s-01-01",year)),"%Y.%m.%d")
#   
#   date.start.month <- seq(as.Date(sprintf("%s-01-01",year)),length=12,by="months")
#   date.end.month <- seq(as.Date(sprintf("%s-02-01",year)),length=12,by="months")-1
#   
#   for (i in 1:12){
#     # url and file info
#     s.date <- format(date.start.month[i],"%Y%m%d")
#     e.date <- format(date.end.month[i],"%Y%m%d")
#     urlZip <- paste0(urlBase,year,
#                      '/bom-tmax_day-',s.date,'-',e.date,".nc")
#     
#     fn <- sprintf("tmax.%s.%02d.nc",as.character(year),i)
#     
#     # download file
#     print(urlZip)
#     try(curl_download_withtest(file.path('downloads/tmax',fn),urlZip))
#   }
# }
# get.tmax.nc.func(2017)

# get met for coords####
get.met.coords.func <- function(year.in,met.nm,lat,lon){
  creat.date.func <- function(year){
    seq(as.Date(sprintf("%s-01-01",year)),
        as.Date(sprintf("%s-12-31",year)),by='day')
  }
  
  read.func <- function(fn){
    
    nc_data <- nc_open(fn)
    
  
    met.var <- paste0(met.nm,'_day')
    
    check.messedup <- try(ncvar_get(nc_data, met.var),silent = TRUE)
    
    if(class(check.messedup) == "try-error"){
      met.var = 'lwe_thickness_of_precipitation_amount'
      lon.vec <- ncvar_get(nc_data, "lon")
      lat.vec <- ncvar_get(nc_data, "lat")
      
    }else{
      lon.vec <- ncvar_get(nc_data, "longitude")
      lat.vec <- ncvar_get(nc_data, "latitude")
    }
    
    met <- ncvar_get(nc_data, met.var)
    
    lat.index <- which.min(abs(lat.vec - lat))    
    lon.index <-  which.min(abs(lon.vec - lon)) 
    
    value.tar <- met[lon.index,lat.index,]
    rm(met)
    return(value.tar)
  }
  
  date.ls <- list()
  for (i in seq_along(year.in)){
    date.ls[[i]] <- data.frame(Date = creat.date.func(year.in[i]))
  }
  
  out.df <- do.call(rbind,date.ls)
  # out.df$year <- year(out.df$Date)
  # out.df$doy <- yday(out.df$Date)
  out.df$lat <- lat
  out.df$lon <- lon
  
  # 
  see <- sapply(year.in, function(y.nm){sprintf("downloads/%s/%s.%s.%02d.nc",met.nm,met.nm,y.nm,1:12)})
  
  file.nm.vec <- as.vector(see)
  
  out.df$temp <- unlist(unname(mapply(read.func,file.nm.vec)))

  names(out.df) <- c('Date','lat','lon',met.nm)
  
  return(out.df)
}

met.wrap.func <- function(year.in,lat,lon){
  evi.ym.df.tmax <- get.met.coords.func(year.in,met.nm='tmax',lat = lat ,lon=lon)
  evi.ym.df.tmin <- get.met.coords.func(year.in,met.nm='tmin',lat = lat ,lon=lon)
  evi.ym.df.rad <- get.met.coords.func(year.in,met.nm='rad',lat = lat ,lon=lon)
  evi.ym.df.rain <- get.met.coords.func(year.in,met.nm='rain',lat = lat ,lon=lon)
  
  evi.ym.df.met <- evi.ym.df.rain
  evi.ym.df.met$tmax <- evi.ym.df.tmax$tmax
  evi.ym.df.met$tmin <- evi.ym.df.tmin$tmin
  evi.ym.df.met$tmean <- (evi.ym.df.met$tmax + evi.ym.df.met$tmin) / 2
  evi.ym.df.met$rad <- evi.ym.df.rad$rad
  return(evi.ym.df.met)
}

# ###############
# get ym
year.in <- 2001:2016
lat <- -33.610412
lon <- 150.73394
met.ym.df <- met.wrap.func(year.in,lat,lon)
saveRDS(met.ym.df,'cache/met.ym20012016.rds')
# 
met.ym.df <- readRDS('cache/met.ym20012016.rds')
evi.ym.df.vp <- get.met.coords.func(year.in,met.nm='vph',lat = lat ,lon=lon)
met.ym.df$vp <- evi.ym.df.vp$vph
saveRDS(met.ym.df,'cache/met.ym20012016.rds')

# get qp
# -26.577250, 144.619028
year.in <- 2001:2016
lat <- -26.577250
lon <- 144.619028
met.qp.df <- met.wrap.func(year.in,lat = lat ,lon=lon)
saveRDS(met.qp.df,'cache/met.qp20012016.rds')


# ng
# -31.645194, 146.641889
year.in <- 2001:2016
lat <- -31.645194
lon <- 146.641889
met.ng.df <- met.wrap.func(year.in,lat = lat ,lon=lon)
saveRDS(met.ng.df,'met.ng20012016.rds')

# DP
# -36.271964, 146.309585
year.in <- 2001:2016
lat <- -36.271964
lon <- 146.309585
met.dp.df <- met.wrap.func(year.in,lat = lat ,lon=lon)
saveRDS(met.dp.df,'met.dp20012016.rds')


# waston sites
watson.sites <- read.csv('watson_site.csv')

save.sites.func <- function(row.watson){
  year.in <- 2001:2016
 
  # tmp.df <- met.wrap.func(year.in,lat = as.numeric(row.watson[2]),lon=as.numeric(row.watson[3]))
  # saveRDS(tmp.df,sprintf('cache/met.%s20012016.rds',row.watson[1]))
  
  # 
  tmp.df <- readRDS(sprintf('cache/met.%s20012016.rds',row.watson[1]))
  evi.ym.df.vp <- get.met.coords.func(year.in,met.nm='vph15',
                                      lat = as.numeric(row.watson[2]),
                                      lon=as.numeric(row.watson[3]))
  tmp.df$vp <- evi.ym.df.vp$vph
  
  vp.sat <- 6.11*exp(17.27*tmp.df$tmax / (237.3+tmp.df$tmax))
  tmp.df$RHmax <- 100 * tmp.df$vp / vp.sat
  
  vp.sat.l <- 6.11*exp(17.27*tmp.df$tmin / (237.3+tmp.df$tmin))
  tmp.df$RHmin <- 100*tmp.df$vp / vp.sat.l
  
  saveRDS(tmp.df,sprintf('cache/met.%s20012016.rds',row.watson[1]))
  
  # print(row.watson[2])
  # print(typeof(row.watson))
  # return(row.watson)
}

save.sites.func <- function(row.watson){
  year.in <- 2001:2016
  
  # tmp.df <- met.wrap.func(year.in,lat = as.numeric(row.watson[2]),lon=as.numeric(row.watson[3]))
  # saveRDS(tmp.df,sprintf('cache/met.%s20012016.rds',row.watson[1]))
  
  # 
  tmp.df <- readRDS(sprintf('cache/met.%s20012016.rds',row.watson[1]))
  # evi.ym.df.vp <- get.met.coords.func(year.in,met.nm='vph15',
  #                                     lat = as.numeric(row.watson[2]),
  #                                     lon=as.numeric(row.watson[3]))
  # tmp.df$vp <- evi.ym.df.vp$vph
  
  # vp.sat <- 6.11*exp(17.27*tmp.df$tmax / (237.3+tmp.df$tmax))
  # tmp.df$RHmax <- 100 * tmp.df$vp / vp.sat
  
  # vp.sat.l <- 6.11*exp(17.27*tmp.df$tmin / (237.3+tmp.df$tmin))
  # tmp.df$RHmin <- 100*tmp.df$vp / vp.sat.l
  names(tmp.df) <- c(c("Date", "lat","lon" ,
                       "Rain","Tmax","Tmin",
                       "Tair" ,"PPFD","vp","RHmin","RHmax"))
  tmp.df$RHmax[tmp.df$RHmax>100] <- 100
  saveRDS(tmp.df,sprintf('cache/met.%s20012016.rds',row.watson[1]))
  
  # print(row.watson[2])
  # print(typeof(row.watson))
  # return(row.watson)
}

apply(watson.sites,1,save.sites.func)
tmp <- readRDS('cache/met.GUNN20012016.rds')
names(tmp)
#######
year.in <- 2001:2016
lat=-16.100000
lon=145.466389

met.cairns.df <- met.wrap.func(year.in,lat = lat ,lon=lon)
saveRDS(met.cairns.df,'met.cairns20012016.rds')

write.csv(met.cairns.df,'cairns.met.csv',row.names = F)
# ####
