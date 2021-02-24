# get gridded climate from bom site
# http://www.bom.gov.au/jsp/awap/rain/index.jsp

# 
# url1 <- "http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/"
# ### file names
# 
# day.list <- seq.Date(as.Date("2020/01/01"),
#                      as.Date("2020/01/31"),
#                      by="day")
# 
# day.list <- gsub("-", "", day.list)
# 
# destDir <- 'downloads/precipitation'
# 

# ### download command
# zip.vec <- c()
# for (i in seq_along(day.list)) {
#   # download.file(url=paste0(url1, i, i, ".grid.Z"),
#   #               destfile=paste0(destDir, "/", i, i, ".grid.Z"))
#   
#   url.tmp <- paste0(url1, day.list[i], day.list[i], ".grid.Z")
#   zip.vec[i] <- paste0(destDir, "/",  day.list[i],  day.list[i], ".grid.Z") 
#   
#   curl_download_withtest( zip.vec[i],url.tmp)
#   
#   # Sys.sleep(2)
#   # read.table('downloads/precipitation/2020010120200101.grid.Z')
# 
# }
# 
# source('r/functions_awap.R')
# 
# Decompress7Zip(zipFileName=sprintf("G:\\repo\\get_evi\\downloads\\precipitation\\2019022120190221.grid.Z"),
#                outputDirectory="G:\\repo\\get_evi\\downloads\\precipitation\\unziped",delete=FALSE)

# get need packages and functions
library(raster)
source('r/functions_awap.R')

# data downloading
get_awap_data('2010-1-1','2020-12-31','maxave','downloads/dailyTmax')

get_awap_data('2010-1-1','2020-12-31','minave','downloads/dailyTmin')

get_awap_data('2010-1-1','2020-12-31','totals','downloads/precipitation')

get_awap_data('2010-1-1','2020-12-31','vprph09','downloads/dailyVP09')

get_awap_data('2010-1-1','2020-12-31','vprph15','downloads/dailyVP15')

get_awap_data('2010-1-1','2020-12-31','solarave','downloads/dailyPAR')

# get awap for coord
get.awap.coord.func <- function(cood.df,met.nm,measure_i,sdate){
  #######################################################
  # inputs:
  # cood.df is the data frame with lat and lon
  # met.nm is the foler name of the met variable;
  # measure_i is the met varible names in the grided data
  # sdate is the date of the met needed
  
  # output:
  # a data frame of the met with coordinates
  ########################################################
  
  fname <- sprintf("downloads/%s/%s_%s%s.grid",met.nm,measure_i,gsub("-","",sdate),gsub("-","",sdate))
  print(fname)
  # data reading
  tmp <- raster(fname)
  sp <- SpatialPoints(cood.df)
  cood.df$met.var <- extract(tmp, sp, method='bilinear')
  cood.df$Date <- sdate
  return(cood.df)
}

# read in modis sites coords
modis.df.tussock <- read.csv('cache/chosen sites.csv')
modis.df.tussock <- modis.df.tussock[seq(1,20,by=2),]
names(modis.df.tussock) <- c('x','y','veg_type','map','map.level')
modis.df.tussock$type <- 'tussock'

modis.df.pasture <- read.csv('cache/chosen pasture sites.csv')
names(modis.df.pasture) <- c('x','y','veg_type','map','map.level')
modis.df.pasture$type <- 'pasture'

modis.site.info.df <- rbind(modis.df.tussock,modis.df.pasture)


# loop through all dates
read.site.met.func <- function(met.nm,measure_i){
  #######################################################
  # inputs:
  # met.nm is the foler name of the met variable;
  # measure_i is the met varible names in the grided data
  
  # output:
  # a data frame of the met with coordinates
  
  # note 
  # date.vec is fixed but should be an input in the future
  ########################################################
  out.nm <- sprintf('cache/met.modis.%s.rds',met.nm)
  
  if(!file.exists(out.nm)){
    
    date.vec <- seq(as.Date('2011-1-1'),
                    as.Date('2020-12-31'),
                    by='day')
    
    tmp.ls <- list()
    for (i in seq_along(date.vec)) {
      tmp.ls[[i]] <- get.awap.coord.func(modis.site.info.df[,c('x','y')],
                                         met.nm,measure_i,
                                         date.vec[i])
      
    }
    tmp.rain.df <- do.call(rbind,tmp.ls)
    saveRDS(tmp.rain.df,out.nm)
  }else{
    tmp.rain.df <- readRDS(out.nm)
  }
 
  return(tmp.rain.df)
}


tmp.rain.df <- read.site.met.func('precipitation','totals')
modis.site.met.df <- merge(modis.site.info.df,tmp.rain.df)
names(modis.site.met.df)[names(modis.site.met.df)=='met.var'] <- 'Rain'

tmp.par.df <- read.site.met.func('dailyPAR','solarave')
tmp.tmax.df <- read.site.met.func('dailyTmax','maxave')
tmp.tmin.df <- read.site.met.func('dailyTmin','minave')

tmp.vp09.df <- read.site.met.func('dailyVP09','vprph09')
tmp.vp15.df <- read.site.met.func('dailyVP15','vprph15')

modis.site.met.df$rad <- tmp.par.df$met.var
modis.site.met.df$tmax <- tmp.tmax.df$met.var
modis.site.met.df$tmin <- tmp.tmin.df$met.var
modis.site.met.df$vp9 <- tmp.vp09.df$met.var
modis.site.met.df$vp15 <- tmp.vp15.df$met.var


vp.sat <- 6.11*exp(17.27*modis.site.met.df$tmin / (237.3+modis.site.met.df$tmin))
modis.site.met.df$RHmax <- 100 * modis.site.met.df$vp9 / vp.sat
modis.site.met.df$RHmax[modis.site.met.df$RHmax>100] <-100

vp.sat.l <- 6.11*exp(17.27*modis.site.met.df$tmax / (237.3+modis.site.met.df$tmax))
modis.site.met.df$RHmin <- 100*modis.site.met.df$vp15 / vp.sat.l
modis.site.met.df$RHmin[modis.site.met.df$RHmin>100] <-100

saveRDS(modis.site.met.df,'cache/modis.met.rds')


