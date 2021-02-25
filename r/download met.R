# The [Data product title] data were obtained through TERN AusCover (http://www.auscover.org.au). TERN is Australia’s land-based ecosystem observatory delivering data streams to enable environmental research and management (TERN, http://www.tern.org.au). TERN is a part of Australia’s National Collaborative Research Infrastructure Strategy (NCRIS, https://www.education.gov.au/national-collaborative-research-infrastructure-strategy-ncris).
# https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f6475_9317_5747_6204
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

for (year.in in 2001:2017) {
  get.met.nc.func(year.in,'tmax')
  # get.met.nc.func(year.in,'tmin')
  # get.met.nc.func(year.in,'rain')
  # get.met.nc.func(year.in,'rad')
  # get.met.nc.func(year.in,'vph15')
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
