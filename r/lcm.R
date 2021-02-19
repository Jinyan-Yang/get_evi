# info##########################################################################
# West Bounding Longitude: 109.504356
# East Bounding Longitude: 157.215737
# North Bounding Latitude: -8.139869
# South Bounding Latitude: -44.318646
# West Bounding Longitude: 109.504356
# East Bounding Longitude: 157.215737
# North Bounding Latitude: -8.139869
# South Bounding Latitude: -44.318646

# keys
# MVG 19. Tussock grasslands
# • Contain a broad range of native grasslands from the blue grass and Mitchell grass communities in the far north to the temperate grasslands of south-eastern Australia.
# MVG 20. Hummock grasslands
# • Hummock forming evergreen perennial grasses that appear as mounds up to 1 m in height, with the ground between hummocks often bare or exposed.
# • Typified by extensive areas of spinifex communities across Australia’s arid lands.
# MVG 21. Other grasslands, herblands, sedgelands and rushlands

# site
# http://environment.gov.au/fed/catalog/search/resource/details.page?uuid=%7B991C36C0-3FEA-4469-8C30-BB56CC2C7772%7D
#####################################################################################

# get color
library(raster)
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

# read lcm
dpath <- 'downloads/lcm/GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/z001001.adf'
lcm.original <-  raster(dpath,layer=1)
# change ocean color
lcm.original@legend@colortable[129] <- '#006994'

lcm.raster <- lcm.original

# set lon and lat
lcm.raster@extent@xmin <- 109.504356
lcm.raster@extent@xmax <- 157.215737
lcm.raster@extent@ymin <- -44.318646
lcm.raster@extent@ymax <- -8.139869
# (-44.318646 +8.139869)/(109.504356-157.215737)
# change color to make the grassland stand out
lcm.raster@legend@colortable[1:256] <- '#808080'#'#FFFDD0'
lcm.raster@legend@colortable[20] <- lcm.original@legend@colortable[20]
lcm.raster@legend@colortable[129] <- '#006994'
# mask NA  with 233 so  aggregation works properly
# lcm.raster <- calc(lcm.raster, fun=function(x){ x[x %in% 1:29 & x!=19&x!=21] <- 0; return(x)})
# range(lcm.rasterc)

# keep the east AU
e <- extent(140, 158, -36, -10)
rc <- crop(lcm.raster, e)

# agre to 2km resolution
rc.agg <- raster::aggregate(rc,10,expand=F,fun = function(x,na.rm=FALSE){
  i=1
  on.exit(return(i))
  if(sum(x == 19) >= 90){
    i=19
  }
  if(sum(x == 21) >= 90){
    i=21
  }
})

# # ceck ag plot
# rc.agg@legend@colortable <- c(rep('#000000',19),col.df$flower[1],'#000000',col.df$flower[2],'#000000')
# plot(rc.agg)
# hist(values(rc.agg),ylim=c(0,10000),breaks = 1:22)
# table(values(rc.agg))

# convert the raster to a df
tmp.df <- as.data.frame(rc.agg,xy=T)

saveRDS(tmp.df,'cache/croped_lcm_df.rds')

# get the saved data.frame
tmp.df <- readRDS('cache/croped_lcm_df.rds')

# get the lc type needed
tmp.df.19 <- tmp.df[tmp.df$layer == 19,]
tmp.df.21 <- tmp.df[tmp.df$layer == 21,]

# # randomly choose points from lat bands
# set.seed(10086)
# random.select.func <- function(tmp.df,n.band = 5,num.per.band = 2){
# 
#   # 
#   # n.band <- 5
#   lon.min <- 140
#   lon.max <- 155
#   
#   brks.vec <- seq(lon.min,lon.max,length.out=n.band+1)
#   # 
#   random.band.func <- function(df,band.min, band.max){
#     
#     band.df <- df[df$x >= band.min & df$x <= band.max,]
#     
#     return(band.df[sample(row.names(band.df),num.per.band),])
#   }
#   
#   tmp.ls <- list()
#   for (i in 2:length(brks.vec)){
#     print(paste0(brks.vec[i-1],'--',brks.vec[i]))
#     tmp.ls[[i-1]] <- random.band.func(tmp.df,brks.vec[i-1],brks.vec[i])
#   }
#   
#   return(do.call(rbind,tmp.ls))
# }
# # not used for now
# # samples.19 <- random.select.func(tmp.df.19,n.band = 5,num.per.band = 1)
# # samples.21 <- random.select.func(tmp.df.21,n.band = 1,num.per.band = 5)

# # randomly selects a few sites
# set.seed(10086)
# r.nm.19 <- sample.int(length(tmp.df.19$x),10)
# # r.nm.21 <- sample.int(length(tmp.df.21$x),10)

# get resolution
map.res<- res(lcm.raster)

# functions to convert coord to x and y

coord2xy.func <- function(coord.vec,is.lat,n.dim,
                          x.min = 109.504356,x.max = 157.215737,y.min = -44.318646,y.max = -8.139869){
  if(is.lat == FALSE){
    return(round((coord.vec  - x.min) / (x.max - x.min) * n.dim))
  }else{
    return(round((coord.vec  - y.min) / (y.max - y.min) * n.dim))
  }
}

# find certre of sites and correct by resolution
if(file.exists('chosen sites.csv')){
  samples.19 <- read.csv('cache/chosen sites.csv')
}else{
  source('r/get_map.R')
  samples.19 <- read.csv('cache/chosen sites.csv')
}
# samples.19 <- tmp.df.19[r.nm.19,]
samples.19$lon <- samples.19$x + map.res[1]*5
samples.19$lat <- samples.19$y + map.res[2]*5
samples.19$x.nm <- coord2xy.func(samples.19$x,is.lat = FALSE,n.dim=lcm.original@ncols)
samples.19$y.nm <-  coord2xy.func(samples.19$y,is.lat = TRUE,n.dim=lcm.original@nrows)
# samples.21 <- tmp.df.21[r.nm.21,]
# samples.21$lon <- samples.21$x + map.res[1]*5
# samples.21$lat <- samples.21$y + map.res[2]*5
# samples.21$x.nm <- coord2xy.func(samples.21$x,is.lat = FALSE,n.dim=lcm.original@ncols)
# samples.21$y.nm <-  coord2xy.func(samples.21$y,is.lat = TRUE,n.dim=lcm.original@nrows)



#make plots
pdf('figures/lcm with sites.pdf',width = 8,height = 1*8)

# plot the hilighted map
plot(lcm.raster)
# adline grid lines
abline(v = 110,lty='dashed',col='darkgrey')
abline(v = 120,lty='dashed',col='darkgrey')
abline(v = 130,lty='dashed',col='darkgrey')
abline(v = 140,lty='dashed',col='darkgrey')
abline(v = 150,lty='dashed',col='darkgrey')
#
abline(h = -10,lty='dashed',col='darkgrey')
abline(h = -20,lty='dashed',col='darkgrey')
abline(h = -30,lty='dashed',col='darkgrey')
abline(h = -40,lty='dashed',col='darkgrey')
# plot choosen sites
# points(x = samples.21$x,y=samples.21$y,pch=0,col='black',cex=.5)
symbol.vec <- as.character(tussock.sample.df$map.level)
points(x = samples.19$x,y=samples.19$y,pch=rep(paste0(0:9),each=2),col='red',cex=1)
# points(x = samples.19$x,y=samples.19$y,pch=0,col='red',cex=.5)
# DN sites
points(x = c(150.73394,144.619028,146.641889,141.712600),
       y = -c(33.610412,26.577250,31.645194,29.607250),
       pch=3,col=col.df$reptile[2],cex=1)
# Waston sites
watson.sites <- read.csv('watson_site.csv')
points(x = watson.sites$lon,y=watson.sites$lat,pch=4,col=col.df$reptile[3],cex=1)
legend(x=111,y=-36.5,legend = c('Tussock','Chosen Tussock','DroughtNet','Watson'),
       pch=c(15,0,3,4),col=c(lcm.original@legend@colortable[20],'red',col.df$reptil[1:2]),bg='grey')

# 

# plot the hilighted map
plot(lcm.raster)
# adline grid lines
abline(v = 110,lty='dashed',col='darkgrey')
abline(v = 120,lty='dashed',col='darkgrey')
abline(v = 130,lty='dashed',col='darkgrey')
abline(v = 140,lty='dashed',col='darkgrey')
abline(v = 150,lty='dashed',col='darkgrey')
#
abline(h = -10,lty='dashed',col='darkgrey')
abline(h = -20,lty='dashed',col='darkgrey')
abline(h = -30,lty='dashed',col='darkgrey')
abline(h = -40,lty='dashed',col='darkgrey')
# plot choosen sites
# points(x = samples.21$x,y=samples.21$y,pch=0,col='black',cex=.5)
symbol.vec <- as.character(tussock.sample.df.tmp$map.level)
points(x = tussock.sample.df.tmp$x,y=tussock.sample.df.tmp$y,pch=rep(paste0(0:9),each=2),col='red',cex=1)
# points(x = samples.19$x,y=samples.19$y,pch=0,col='red',cex=.5)
# DN sites
points(x = c(150.73394,144.619028,146.641889,141.712600),
       y = -c(33.610412,26.577250,31.645194,29.607250),
       pch=3,col=col.df$reptile[2],cex=1)
# Waston sites
watson.sites <- read.csv('watson_site.csv')
points(x = watson.sites$lon,y=watson.sites$lat,pch=4,col=col.df$reptile[3],cex=1)
legend(x=111,y=-36.5,legend = c('Tussock','Chosen Tussock','DroughtNet','Watson'),
       pch=c(15,0,3,4),col=c(lcm.original@legend@colortable[20],'red',col.df$reptil[1:2]),bg='grey')

# 

plot(lcm.original,asp=1)
# plot choosen sites

points(x = samples.19$x.nm,y=samples.19$y.nm,pch=0,col='red',cex=.5)
# points(x = samples.21$x.nm,y=samples.21$y.nm,pch=0,col='black',cex=.5)

# DN sites
points(x = coord2xy.func(c(150.73394,144.619028,146.641889,141.712600),is.lat = FALSE,n.dim=lcm.original@ncols),
       y = coord2xy.func(-c(33.610412,26.577250,31.645194,29.607250),is.lat = T,n.dim=lcm.original@nrows),
       pch=3,col=col.df$reptile[2],cex=1)
# Waston sites
watson.sites <- read.csv('watson_site.csv')

points(x = coord2xy.func(watson.sites$lon,is.lat = FALSE,n.dim=lcm.original@ncols),
       y = coord2xy.func(watson.sites$lat,is.lat = TRUE,n.dim=lcm.original@nrows),
       pch=4,col=col.df$reptile[3],cex=1)

legend(x=100,y=4500,legend = c('Tussock','Chosen Tussock','DroughtNet','Watson'),
       pch=c(15,0,3,4),col=c(lcm.original@legend@colortable[20],'red',col.df$reptil[1:2]),bg='grey')

dev.off()

