# https://lpdaac.usgs.gov/products/mcd43a4v006/
# library(curl)####
library(raster)
library(ncdf4)
library(gdalUtils)
library(R.utils)
library(lubridate)
library(geosphere)
source('r/reprojection_func.R')

# lat= -31.645194
# lon=146.641889
# source('r/get_DN_met.R')
#function to  downloaded file from url with test ####
curl_download_withtest <- function(filename, url,...){
  if(!file.exists(filename)){
    download.file(url, filename,"curl",quiet = FALSE,...)
  } else {
    message("Skipped ",filename, "; already exists.")
  }
}

get.modis.fc.func <- function(year){
  # http://remote-sensing.nci.org.au/u39/public/data/modis/lpdaac-mosaics-cmar/v1-hdf4/aust/MOD13Q1.005/
  # 2000.02.18/MOD13Q1.2000.049.aust.005.b02.250m_evi.hdf.gz
  # http://dapds00.nci.org.au/thredds/fileServer/tc43/modis-fc/v310/tiles/8-day/cover/FC.v310.MCD43A4.h01v10.2014.006.nc
  urlBase <- 'http://dapds00.nci.org.au/thredds/fileServer/tc43/modis-fc/v310/tiles/8-day/cover/'
  
  tile.df <- data.frame(h = sprintf('%02d',rep(27:32,each=6)),
                        v = sprintf('%02d',9:14))
  
  fn.vec <- paste0('FC.v310.MCD43A4.h',
               tile.df$h,'v', tile.df$v,
               '.',year,'.006.nc')

  
  for (i in seq_along(fn.vec)){
    # url and file info
    get.nm <-fn.vec[i] 
    get.url <- paste0(urlBase,get.nm)
    # download file
    try(curl_download_withtest(file.path('downloads','modisFC',get.nm),get.url))
    
  }
  
}
# download modis fc
get.modis.fc.func(2020)
get.modis.fc.func(2019)
get.modis.fc.func(2014)
get.modis.fc.func(2015)

# get the tile of the coord####
get.tile.func <-function(lat,lon,year){
  
  tile.df <- data.frame(h = sprintf('%02d',rep(27:32,each=6)),
                        v = sprintf('%02d',9:14))
  
  fn.vec <- paste0('FC.v310.MCD43A4.h',
                   tile.df$h,'v', tile.df$v,
                   '.',year,'.006.nc')
  # 
  found.check = 0
  i=1
  tmp.nm.vec <- c()
  dist.vec <- c()
  while(found.check<10){
    fn <- file.path('downloads','modisFC',fn.vec[i])
    
    nc_data <- try(nc_open(fn),silent = TRUE)
    
    if(class(nc_data) !=  "try-error"){
      lon.vec <- ncvar_get(nc_data, "x")
      lat.vec <- ncvar_get(nc_data, "y")
      
      DF <- data.frame(x = lon.vec,y=lat.vec)
      coords.repro.df <- repro.modis.fun(DF)
      
      lon.check <- range(coords.repro.df$x) - lon
      lat.check <- range(coords.repro.df$y) - lat
      
      if(lon.check[1] <= 0 & lon.check[2] >= 0 & 
         lat.check[1] <= 0 & lat.check[2] >= 0){
        found.check = 1 + found.check
        tmp.nm.vec[found.check] <- fn
        
        dist.vec[found.check] <- min(abs(lon.check))  + min(abs(lat.check)) 
        # target.nm <- fn
      }else{
        found.check = 0
        i=i+1
      }
    }else{
      i=i+1
      found.check = found.check
    }
  }
  
  target.nm <- tmp.nm.vec[which.min(dist.vec)]
  
  if(!file.exists('cache/coorinfor.rds')){
    
    df <- data.frame(lat = lat,lon=lon,year=year,fn=target.nm)
    saveRDS(df,'cache/coorinfor.rds')

  }else{
    con <- file("cache/coorinfor.rds")
    df <- readRDS(con)
    df.add <- data.frame(lat = lat,lon=lon,year=year,fn=target.nm)
    df.new <- rbind(df,df.add)
    saveRDS(df.new, con)
  }
  
  return(target.nm)
  
}

# function to get fc based on coord####
get.fc.coords.ns.func <- function(year.in,lat,lon,site.nm=''){
  
  
  # tile.df <- data.frame(h = sprintf('%02d',rep(0:35,10)),
  #                       v = sprintf('%02d',1:10))

  # s.tile.nm <- 'h28v12'
  # 
  # fn <- paste0('FC.v310.MCD43A4.',
  #                  s.tile.nm,# tile.df$h,'v', tile.df$v,
  #                  '.',year,'.006.nc')
  # fn <- paste0('downloads/modisFC/',fn)
  
  for (i in 1:length(year.in)){
    
    year = year.in[i]
    
    rds.nm <- paste0('cache/',site.nm,year,'.rds')
    
    if(!file.exists(rds.nm)){
      if(!file.exists('cache/coorinfor.rds')){
        fn <- get.tile.func(lat,lon,year)
      }else{
        nm.df <- readRDS('cache/coorinfor.rds')
        fn.tmp <- nm.df$fn[nm.df$lon == lon & nm.df$lat == lat & nm.df$year == year]
        
        if(length(fn.tmp)<1){
          fn <- get.tile.func(lat,lon,year)
        }else{
          fn <- as.character(fn.tmp)
        }
        
      }
      
      
      print(c('Try read:',fn))
      nc_data <- nc_open(fn)
      # print(nc_data)
      lon.vec <- ncvar_get(nc_data, "x")# * km2degree
     
      lat.vec <- ncvar_get(nc_data, "y")# * km2degree
      
      DF <- data.frame(x = lon.vec,y=lat.vec)
      coords.repro.df <- repro.modis.fun(DF)

      time.vec <- ncvar_get(nc_data, "time")
      class(time.vec) = c('POSIXct')
      
      # range(lon.vec)
      # range(lat.vec)
      
      # live cover
      fc <- ncvar_get(nc_data, "phot_veg")
      # dead cover
      dc <- ncvar_get(nc_data, "nphot_veg")
      
      # get value for coord
      # x.dist= distm(c(lon,lat),c(0,lat),fun = distHaversine)[1]
      # y.dist=-distm(c(lon,lat),c(lon,0),fun = distHaversine)[1]
      
      lat.index <- which.min(abs(coords.repro.df$y - lat))    
      lon.index <-  which.min(abs(coords.repro.df$x - lon)) 
      
      # # MAKE A PLOT TO CHECK POINT LOCATION
      # plot(raster((fc[,,1]),xmn = min(lon.vec),xmx = max(lon.vec),ymn = min(lat.vec),ymx = max(lat.vec)))
      # 
      # x.dist= distm(c(lon,lat),c(0,lat),fun = distHaversine)
      # 
      # y.dist=distm(c(lon,lat),c(lon,0),fun = distHaversine)
      # 
      # points(x = x.dist,y=-y.dist,pch=16,col='red')
      
      value.tar <- fc[lon.index,lat.index,]
      value.tar.df <- dc[lon.index,lat.index,]
      
      # clear memory
      rm(dc)
      rm(fc)
      rm(nc_data)
      
      # save file
      out.df <- data.frame(fn = fn,
                           lat = lat,lon=lon,site = site.nm,
                           DateTime = time.vec,Date = as.Date(time.vec),
                           FC = value.tar,DC = value.tar.df
      )
      
      
      
      saveRDS(out.df,rds.nm)
    }
    
  }

}

# get fc for DN sites####
DN.site.info.df <- data.frame(site = c('NG','MP','QP','YM'),
                              lat = -c(31.645194,29.607250,26.577250,33.613540 ),
                              lon = c(146.641889,141.712600,144.619028,150.738132))
# DN.site.info.df$tile.nm <- NA
# 
# for (i in 1:nrow(DN.site.info.df)) {
# 
#   DN.site.info.df$tile.nm[i] <- get.tile.func(DN.site.info.df$lat[i],
#                                            DN.site.info.df$lon[i],2020)
# }

for (i in 1:nrow(DN.site.info.df)){
  get.fc.coords.ns.func(year.in = c(2019,2020),
                        lat = DN.site.info.df$lat[i],
                        lon = DN.site.info.df$lon[i],
                        site.nm = DN.site.info.df$site[i])
  
  
}

# get fc for cw sites ######
watson.sites <- read.csv('watson_site_new.csv')
for (i in 1:nrow(watson.sites)) {
  get.fc.coords.ns.func(year.in = c(2014,2015),
                        lat = watson.sites$lat[i],
                        lon = watson.sites$lon[i],
                        site.nm = watson.sites$code[i])
}


# make plots#####
pdf('DN.plantCover.pdf',width = 8,height = 8*.618)

palette(rainbow(8))

tmp.ls <- list()
for (i in 1:nrow(DN.site.info.df)) {
  rds.nm.19 <- paste0('cache/',DN.site.info.df$site[i],2019,'.rds')
  rds.nm.20 <- paste0('cache/',DN.site.info.df$site[i],2020,'.rds')
  
  tmp.19 <- readRDS(rds.nm.19)
  tmp.20 <- readRDS(rds.nm.20)
  
  tmp.ls[[i]] <- rbind(tmp.19,tmp.20)
  
  if(i ==1){
    plot(FC~Date,data =  tmp.ls[[i]],col = i,type='b',ylim=c(0,100),pch=16,
         xlab=' ',ylab='Plant cover (%)',xaxt='n')
    
    # 
    date.range = range(tmp.ls[[i]]$Date,na.rm=T)
    mons.vec =  seq(date.range[1],date.range[2],by='mon')
    
    mon.c <- format(mons.vec,'%m')
    axis(1,at = mons.vec,labels = mon.c)
    # mtext('2018',side = 1,adj=0,line = 3)
    # mtext('2019',side = 1,adj=0.5,line = 3)
    yr.vec <- unique(year(tmp.ls[[i]]$Date))
    where.c <-which(mon.c =='01') / length(mon.c)
    num.yr <- length(where.c)
    mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
    
    # plot.rain.func(ng.swc.df,DN.site.info.df$site[i],past.days = 365,)
    
  }else{
    points(FC~Date,data =  tmp.ls[[i]],col = i,type='b',pch=16)
  }
  
  legend('topleft',legend = unique(DN.site.info.df$site),pch=16,col=palette(),bty='n')
}

title('MODIS Fractional Cover',line=2)
dev.off()

