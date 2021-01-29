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
dpath <- 'downloads/lcm/GRID_NVIS5_1_AUST_EXT_MVG/aus5_1e_mvg.ovr'
lcm.original <-  raster(dpath,layer=1)
lcm.raster <- lcm.original

# set lon and lat
lcm.raster@extent@xmin <- 109.504356
lcm.raster@extent@xmax <- 157.215737
lcm.raster@extent@ymin <- -44.318646
lcm.raster@extent@ymax <- -8.139869
# (-44.318646 +8.139869)/(109.504356-157.215737)

# change color to make the grassland stand out
lcm.raster@legend@colortable[1:256] <- '#FFFDD0'#A9A9A9'
lcm.raster@legend@colortable[129] <- '#006994'
lcm.raster@legend@colortable[c(20,22)] <- col.df$flower[1:2]
# mask NA  with 233 so  aggregation works properly
# lcm.raster <- calc(lcm.raster, fun=function(x){ x[x %in% 1:29 & x!=19&x!=21] <- 0; return(x)})
# range(lcm.rasterc)

# keep the east AU
e <- extent(140, 158, -36, -10)
rc <- crop(lcm.rasterc, e)

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

saveRDS(tmp.df,'croped_lcm_df.rds')

# get the saved data.frame
tmp.df <- readRDS('croped_lcm_df.rds')

# get the lc type needed
tmp.df.19 <- tmp.df[tmp.df$layer == 19,]
tmp.df.21 <- tmp.df[tmp.df$layer == 21,]

# randomly choose points from lat bands
set.seed(10086)
random.select.func <- function(tmp.df,n.band = 5,num.per.band = 2){

  # 
  # n.band <- 5
  lon.min <- 140
  lon.max <- 155
  
  brks.vec <- seq(lon.min,lon.max,length.out=n.band+1)
  # 
  random.band.func <- function(df,band.min, band.max){
    
    band.df <- df[df$x >= band.min & df$x <= band.max,]
    
    return(band.df[sample(row.names(band.df),num.per.band),])
  }
  
  tmp.ls <- list()
  for (i in 2:length(brks.vec)){
    print(paste0(brks.vec[i-1],'--',brks.vec[i]))
    tmp.ls[[i-1]] <- random.band.func(tmp.df,brks.vec[i-1],brks.vec[i])
  }
  
  return(do.call(rbind,tmp.ls))
}
# not used for now
# samples.19 <- random.select.func(tmp.df.19,n.band = 5,num.per.band = 1)
# samples.21 <- random.select.func(tmp.df.21,n.band = 1,num.per.band = 5)

# randomly selects a few sites
set.seed(1111)
r.nm.19 <- sample.int(length(tmp.df.19$x),10)
r.nm.21 <- sample.int(length(tmp.df.21$x),10)

# get resolution
map.res<- res(lcm.raster)

# find certre of sites and correct by resolution
samples.19 <- tmp.df.19[r.nm.19,]
samples.19$lon <- samples.19$x + map.res[1]*5
samples.19$lat <- samples.19$y + map.res[2]*5
samples.21 <- tmp.df.21[r.nm.21,]
samples.21$lon <- samples.21$x + map.res[1]*5
samples.21$lat <- samples.21$y + map.res[2]*5
# save chosen sites
write.csv(rbind(samples.19,samples.21),'chosen sites.csv',row.names = F)


#make plots
pdf('lcm with sites.pdf',width = 8,height = 0.76*8)
# 
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
points(x = samples.21$x,y=samples.19$y,pch=0,col='black',cex=.5)
points(x = samples.19$x,y=samples.21$y,pch=0,col='red',cex=.5)
# DN sites
points(x = c(150.73394,144.619028,146.641889,141.712600),
       y = -c(33.610412,26.577250,31.645194,29.607250),
       pch=3,col=col.df$reptile[2],cex=1)
# Waston sites
watson.sites <- read.csv('watson_site.csv')
points(x = watson.sites$lon,y=watson.sites$lat,pch=4,col=col.df$reptile[3],cex=1)

legend(x=109.504356,y=-35,legend = c('Tussock','Other','choosen Tussock','choosen other','DroughtNet','Watson'),
       pch=c(15,15,0,0,3,4),col=c(col.df$flower[1:2],'red','black',col.df$reptil[1:2]))

# plot the original map
plot(lcm.original)
# legend('bottomleft',)
dev.off()

