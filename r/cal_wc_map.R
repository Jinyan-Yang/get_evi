# read in modis sites coords
modis.df.tussock <- read.csv('cache/chosen sites.csv')
modis.df.tussock <- modis.df.tussock[seq(1,20,by=2),]
names(modis.df.tussock) <- c('x','y','veg_type','map','map.level')
modis.df.tussock$type <- 'tussock'

modis.df.pasture <- read.csv('cache/chosen pasture sites.csv')
names(modis.df.pasture) <- c('x','y','veg_type','map','map.level')
modis.df.pasture$type <- 'pasture'

modis.site.info.df <- rbind(modis.df.tussock,modis.df.pasture)

# readwc map

url <- 'http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_prec.zip'
download.file(url,'downloads/wc_prec.zip')

unzip('downloads/wc_prec.zip',exdir = 'downloads/wc_map')

map.fn <- list.files('downloads/wc_map',full.names = T,pattern = '.tif')

library(raster)
library(abind)
temp.ls <- list()
for (i in seq_along(map.fn)){

  temp.ra <- raster(map.fn[i])
  temp.ra@extent@xmin <- -180
  temp.ra@extent@xmax <- 180
  temp.ra@extent@ymin <- -90
  temp.ra@extent@ymax <- 90
  
  e <- extent(140, 158, -36, -10)
  rc <- crop(temp.ra, e)
  
  

  temp.ls[[i]] <- as.matrix(rc)
}

tmp.array <- abind(temp.ls,along=3)
# get mean of 12 months
map.wc.raster <- rowMeans(tmp.array, dims=2)*12
map.wc.raster <- raster(map.wc.raster)
map.wc.raster@extent@xmin <- 140
map.wc.raster@extent@xmax <- 158
map.wc.raster@extent@ymin <- -36
map.wc.raster@extent@ymax <- -10
# plot(raster(map.wc.raster))
saveRDS(map.wc.raster,'cache/wc.map.auEast.30s.rds')


# 
# map.wc.raster <- readRDS('downloads/wc.map.rds')
map.wc.raster <- readRDS('cache/wc.map.auEast.30s.rds')

sp <- SpatialPoints(modis.site.info.df[,c('x','y')])
modis.site.info.df$map.wc <- extract(map.wc.raster, sp, method='bilinear')
