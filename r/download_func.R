# library(curl)
library(raster)
library(ncdf4)
library(gdalUtils)
library(R.utils)
library(lubridate)
#function to  downloaded file from url with test 
curl_download_withtest <- function(filename, url,...){
  if(!file.exists(filename)){
    download.file(url, filename,"curl",quiet = FALSE,...)
  } else {
    message("Skipped ",filename, "; already exists.")
  }
}

get.modis.func <- function(year){
  # http://remote-sensing.nci.org.au/u39/public/data/modis/lpdaac-mosaics-cmar/v1-hdf4/aust/MOD13Q1.005/
  # 2000.02.18/MOD13Q1.2000.049.aust.005.b02.250m_evi.hdf.gz

  urlBase <- 'http://remote-sensing.nci.org.au/u39/public/data/modis/lpdaac-mosaics-cmar/v1-hdf4/aust/MOD13Q1.005/'
  
  
  yearDate <-  format(as.Date(sprintf("%s-01-01",year)),"%Y.%m.%d")
  date.seq <- seq(as.Date(sprintf("%s-01-01",year)),
                  as.Date(sprintf("%s-12-31",year)),by=16)
  doy  <-  yday(date.seq)
  
  for (i in seq_along(doy)){
    # url and file info
    urlZip <- paste0(urlBase,format(date.seq[i],"%Y.%m.%d"),
                     "/MOD13Q1.",year,".",sprintf("%03d.aust.005.", doy[i]),"b02.250m_evi.hdf.gz")
    # MOD15A2.2004.073.aust.005.b03.1000m_quality.hdf.gz
    
    # http://remote-sensing.nci.org.au/u39/public/data/modis/lpdaac-mosaics-cmar/v1-hdf4/aust/MOD13Q1.005/
    # 2000.02.18/MOD13Q1.2000.049.aust.005.b03.250m_vi_quality.hdf.gz
    url.qa <- paste0(urlBase,format(date.seq[i],"%Y.%m.%d"),
                     "/MOD13Q1.",year,".",sprintf("%03d.aust.005.", doy[i]),"b03.250m_vi_quality.hdf.gz")
    # down.path <- file.path("downloads","modis",paste0(yearDate))
    # qa.path <-  file.path("downloads","modis","qa",paste0(yearDate))

    fn <- sprintf("MOD.%s.%03d.250m_evi.hdf.gz",as.character(year),doy[i])
    qa.fn <- sprintf("MOD.%s.%03d.250m_qa.hdf.gz",as.character(year),doy[i])
    # curl_download(urlZip,fn)
    # download file
    try(curl_download_withtest(file.path('downloads',fn),urlZip))
    try(curl_download_withtest(file.path('downloads',qa.fn),url.qa))
    
    
    #   # uncompress file
    uz.fn <- sprintf("MOD.%s.%03d.250m_evi.hdf",as.character(year),doy[i])
    uz.qa.fn <- sprintf("MOD.%s.%03d.250m_qa.hdf",as.character(year),doy[i])
    
      if(!file.exists(file.path('downloads',uz.fn))){
        try(gunzip(file.path('downloads',fn),destname=file.path('downloads',uz.fn),remove=FALSE))
      } else {
        message("Skipped ",file.path('downloads',uz.fn), "; already exists.")
      }
    
      if(!file.exists(file.path('downloads',uz.qa.fn))){
        try(gunzip(file.path('downloads',qa.fn),destname=file.path('downloads',uz.qa.fn),remove=FALSE))
      } else {
        message("Skipped ",file.path('downloads',uz.qa.fn), "; already exists.")
      }
    
    # make files into tif to be read in later
    out.fn <- file.path("downloads","modis",'evi',paste0('evi_',year,doy[i]))
    out.qa.fn <- file.path("downloads","modis","qa",paste0('qa_',year,doy[i]))
    filename <- paste0(out.fn,".tif")
    filename.qa <- paste0(out.qa.fn,".tif")
    
      if(!file.exists(filename)){
        try(gdal_translate(file.path('downloads',uz.fn), dst_dataset = filename))
      } else {
        message("Skipped ",filename, "; already exists.")
      }

      
      if(!file.exists(filename.qa)){
        try(gdal_translate(file.path('downloads',uz.qa.fn), dst_dataset = filename.qa))
      } else {
        message("Skipped ",filename.qa, "; already exists.")
      }
  }
  
  #   # uncompress file
    # out.fn <- file.path("downloads","modis",'evi')
    # out.qa.fn <- file.path("downloads","modis","qa",paste0('qa_',year,doy,'.hdf'))
  #   
  # 
  #   if(!file.exists(out.fn)){
  #     try(gunzip(fn,destname=out.fn,remove=FALSE))
  #   } else {
  #     message("Skipped ",out.fn, "; already exists.")
  #   }
  #   
  #   if(!file.exists(out.qa.fn)){
  #     try(gunzip(qa.fn,destname=out.qa.fn,remove=FALSE))
  #   } else {
  #     message("Skipped ",out.qa.fn, "; already exists.")
  #   }
  #   # make files into tif to be read in later
  #   filename <- paste0(out.fn,".tif")
  #   
  #   if(!file.exists(filename)){
  #     try(gdal_translate(out.fn, dst_dataset = filename))
  #   } else {
  #     message("Skipped ",filename, "; already exists.")
  #   }
  #   
  #   filename.qa <- paste0(out.qa.fn,".tif")
  #   if(!file.exists(filename.qa)){
  #     try(gdal_translate(out.qa.fn, dst_dataset = filename.qa))
  #   } else {
  #     message("Skipped ",filename.qa, "; already exists.")
  #   }
  #   
  #   yearDate <- format(as.Date(yearDate,"%Y.%m.%d") + 8,"%Y.%m.%d")
  # }
  # 
  # file.list <- list.files(file.path("downloads","modis"),pattern = c(as.character(year),"[.]tif"))
  # file.list <- file.list[grep(".tif", file.list, fixed=T)]
  # 
  # qa.file.list <- list.files(file.path("downloads","modis","qa"),pattern = c(as.character(year),"[.]tif"))
  # qa.file.list <- qa.file.list[grep(".tif", qa.file.list, fixed=T)]
  # 
  # raster.ls <- list()
  # 
  # 
  # for (i in 1:length(file.list)){
  #   raster.ls[[i]] <- (raster(file.path("downloads","modis",file.list[i])))
  #   raster.qa.ls<- raster(file.path("downloads","modis","qa",qa.file.list[i]))
  #   
  #   raster.qa.ls <- as.matrix(raster.qa.ls)
  #   raster.ls[[i]] <- as.matrix(raster.ls[[i]])
  #   # these two values report the best quality data
  #   
  #   sel.index <- which(raster.qa.ls != 0)
  #   
  #   raster.ls[[i]][sel.index] <- NA
  #   
  #   raster.ls[[i]][raster.ls[[i]] > 10] <- NA
  # }
  # 
  # # modis_array <- abind(raster.ls, along=3)
  # # 
  # # modis_annual <- rowMeans(modis_array,na.rm = TRUE,dims=2)
  # # # modis_annual[which(modis_annual > 6)] <- NA
  # annual.mean.fn <- file.path("cache",paste0(year,".rds"))
  # 
  # saveRDS(modis_array,file=annual.mean.fn)
}


# get.modis.func(2001)

for(year in 2001:2011){
  get.modis.func(year)
}
# get.modis.func(2016)
