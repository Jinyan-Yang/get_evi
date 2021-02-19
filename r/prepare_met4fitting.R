library(ncdf4)
library(lubridate)
# get met for coords####
get.met.coords.func <- function(year.in,met.nm,lat,lon){
  creat.date.func <- function(year){
    seq(as.Date(sprintf("%s-01-01",year)),
        as.Date(sprintf("%s-12-31",year)),by='day')
  }
  
  read.func <- function(fn){
    
    nc_data <- try(nc_open(fn),silent = TRUE)
    
    if(class(nc_data) == "try-error"){
      return(NA)
      
    }
    
    if(met.nm == "rain"){
      met.var = 'lwe_thickness_of_precipitation_amount'
      
    }else{
      met.var <- paste0(met.nm,'_day')
    }
    
    check.messedup <- try(ncvar_get(nc_data, met.var),silent = TRUE)
    
    if(class(check.messedup) == "try-error"){
      value.tar <- NA
    }else{
      met <- ncvar_get(nc_data, met.var)
      
      if(met.nm == "rain"){
        lon.vec <- ncvar_get(nc_data, "lon")
        lat.vec <- ncvar_get(nc_data, "lat")
      }else{
        lon.vec <- ncvar_get(nc_data, "longitude")
        lat.vec <- ncvar_get(nc_data, "latitude")
      }
     
      lat.index <- which.min(abs(lat.vec - lat))    
      lon.index <-  which.min(abs(lon.vec - lon)) 
      
      value.tar <- met[lon.index,lat.index,]
      rm(met)
    }
    
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
  read.met.func <- function(y.nm){
    
    s.date <- as.Date(sprintf('%s-1-1',y.nm))
    e.date <- as.Date(sprintf('%s-12-1',y.nm))
    date.vec <- seq(s.date,e.date,by='day')
    
    out.ls <- list()
    
    for (i in 1:12) {
      fn <- sprintf("downloads/%s/%s.%s.%02d.nc",met.nm,met.nm,y.nm,i)
      met.val <- read.func(fn)
      
      out.ls[[i]] <- data.frame(Date = date.vec[month(date.vec) == i],
                                lat = lat,
                                lon = lon,
                                temp = met.val)
  
      
    }
    out.df <- do.call(rbind,out.ls)
    
    return(out.df)
  }
  # fn.vec <- sapply(year.in, function(y.nm){sprintf("downloads/%s/%s.%s.%02d.nc",met.nm,met.nm,y.nm,1:12)})
  # 
  # fn.vec <- as.vector(fn.vec)
  # 
  # out.df$temp <- unlist(unname(mapply(read.func,fn.vec)))
  
  met.ls <- lapply(year.in, read.met.func)
  
  out.df <- do.call(rbind,met.ls)
  
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
# tmp <- readRDS('cache/met.GUNN20012016.rds')
# names(tmp)
# ######
year.in <- 2001:2016
lat=-16.100000
lon=145.466389

met.cairns.df <- met.wrap.func(year.in,lat = lat ,lon=lon)
saveRDS(met.cairns.df,'met.cairns20012016.rds')

write.csv(met.cairns.df,'cairns.met.csv',row.names = F)

# get met for satellite data####

modis.df.tussock <- read.csv('cache/chosen sites.csv')
modis.df.tussock <- modis.df.tussock[seq(1,20,by=2),]
names(modis.df.tussock) <- c('x','y','veg_type','map','map.level')
modis.df.pasture <- read.csv('cache/chosen pasture sites.csv')
names(modis.df.pasture) <- c('x','y','veg_type','map','map.level')

modis.df <- rbind(modis.df.tussock,modis.df.pasture)

year.in <- 2007:2016

tmp.ls <- list()

for (i in 1:nrow(modis.df)){
  
  tmp.df <- met.wrap.func(year.in,lat = modis.df$y[i],lon = modis.df$x[i])
  
  tmp.df$vp <- tmp.df$vph
  
  vp.sat <- 6.11*exp(17.27*tmp.df$tmax / (237.3+tmp.df$tmax))
  tmp.df$RHmax <- 100 * tmp.df$vp / vp.sat
  
  vp.sat.l <- 6.11*exp(17.27*tmp.df$tmin / (237.3+tmp.df$tmin))
  tmp.df$RHmin <- 100*tmp.df$vp / vp.sat.l
  
  tmp.ls[[i]] <- tmp.df
}
