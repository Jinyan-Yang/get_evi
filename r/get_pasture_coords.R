# link
# https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/83868

library(raster)
# read lcm
dpath <- 'downloads/lcm/DLCD_v2-1_MODIS_EVI_13_20140101-20151231.tif'
lcm.modis <-  raster(dpath,layer=1)

# keep the east AU
e <- extent(140, 158, -36, -10)
rc <- crop(lcm.modis, e)

# # set lon and lat
# lcm.modis@extent@xmin <- 109.504356
# lcm.raster@extent@xmax <- 157.215737
# lcm.raster@extent@ymin <- -44.318646
# lcm.raster@extent@ymax <- -8.139869
# agre to 2km resolution
rc.agg <- raster::aggregate(rc,10,expand=F,fun = function(x,na.rm=FALSE){
  i=1
  on.exit(return(i))
  
  check.sum <- sum(x == 9,na.rm=T)
  
  if(is.na(check.sum)){
    i=100
  }else{
    
    if(check.sum >= 90){
      i=9
    }
  }
})

# plot(lcm.modis)
tmp.df <- as.data.frame(rc.agg,xy=T)

# get the lc type needed
tmp.df.pasture <- tmp.df[tmp.df$DLCD_v2.1_MODIS_EVI_13_20140101.20151231 == 9,]

saveRDS(tmp.df.pasture,'cache/croped_pasture_df.rds')
