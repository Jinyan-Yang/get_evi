# get the land cover map of australia 2005-2006 from the site 
# url <- "http://data.daff.gov.au/data/warehouse/luav4g9abl078/luav4g9abl07811a02egigeo___.zip"
# fn <- "lcm20052006.zip"
# out.dir <- "downloads/LCM"
# f.path <- file.path(out.dir,fn)
# curl_download_withtest(f.path,url)
# 
# unzip(f.path,exdir = out.dir)

# dpath <- file.path(out.dir,"luav4g9abll07811a02egigeo___","lu05v4ag","w001001.adf")

# West Bounding Longitude: 109.504356
# East Bounding Longitude: 157.215737
# North Bounding Latitude: -8.139869
# South Bounding Latitude: -44.318646
# West Bounding Longitude: 109.504356
# East Bounding Longitude: 157.215737
# North Bounding Latitude: -8.139869
# South Bounding Latitude: -44.318646
# http://environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B991C36C0-3FEA-4469-8C30-BB56CC2C7772%7D
library(raster)
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")


dpath <- 'downloads/lcm/GRID_NVIS5_1_AUST_EXT_MVG/aus5_1e_mvg.ovr'
lcm.raster <- raster(dpath,layer=1)

# xmin = 109.504356,xmax = 157.215737,ymin = -44.318646,ymax = -8.139869
lcm.raster@extent@xmin <- 109.504356
lcm.raster@extent@xmax <- 157.215737
lcm.raster@extent@ymin <- -44.318646
lcm.raster@extent@ymax <- -8.139869

# 
lcm.raster@legend@colortable[1:256] <- '#A9A9A9'
lcm.raster@legend@colortable[129] <- '#006994'
# lcm.raster@legend@colortable[25] <- '#87ceeb'
# lcm.raster@legend@colortable[2] <-'#228B22'
lcm.raster@legend@colortable[c(20,22)] <-col.df$flower[1:2]

plot(lcm.raster)
points(14000,10000,pch=16,col='black')
lcm.raster[14000,10000]
which(lcm.raster == 20)



# 
lon.vec <- seq(109.504356,157.215737,length.out = lcm.raster@ncols)
lat.vec <- seq(-44.318646,-8.139869,length.out = lcm.raster@nrows)





attributes.df <- lcm.raster@data@attributes











lcm.matrix <-as.matrix(lcm.raster)
lcm.raster@crs@projargs
long.lcm <- seq(lcm.raster@extent[1]+0.01,lcm.raster@extent[2],0.01)
lati.lcm <- rev(seq(lcm.raster@extent[3],lcm.raster@extent[4]-0.01,0.01))

# make the lad cover type a raster
attributes.df <- lcm.raster@data@attributes[[1]]
lcm.vec <- as.vector(lcm.matrix)
nona.index <- which(is.na(lcm.vec) == FALSE)

# lc.type.vec <- rep(attributes.df$LU_CODE,attributes.df$COUNT)

lc.na.vec <- rep(NA,length(lcm.vec))
lc.na.vec[nona.index] <- attributes.df$LU_CODE[lcm.vec[nona.index]]

lc.type.m <- matrix(lc.na.vec,ncol=ncol(lcm.matrix),nrow=nrow(lcm.matrix),byrow=FALSE)

# get the data inline with climate
# coord for vpd
lati <- ncvar_get(nc_open("downloads/rainfall/rainfall2002.nc"), "latitude")
long <- ncvar_get(nc_open("downloads/rainfall/rainfall2002.nc"), "longitude")
# find closest value
num.lati <- c()
for (i in 1:length(lati)){
  num.lati[i] <- which(abs(lati[i]-lati.lcm)==min(abs(lati[i]-lati.lcm)))
}

num.longi <- c()
for (i in 1:length(long)){
  num.longi[i] <- which(abs(long[i]-long.lcm)==min(abs(long[i]-long.lcm)))
}

# subset map with climate data: it was transponded and reversed to fit the format of the climate
lc.sub.m <- t(lc.type.m[(num.lati),(num.longi)])
saveRDS(lc.sub.m,"output/data/lcm.rds")