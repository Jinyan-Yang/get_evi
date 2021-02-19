# function to covert gps to map coords
coord2xy.func <- function(coord.vec,is.lat,n.dim,
                          x.min = 109.504356,x.max = 157.215737,y.min = -44.318646,y.max = -8.139869){
  if(is.lat == FALSE){
    return(round((coord.vec  - x.min) / (x.max - x.min) * n.dim))
  }else{
    return(round((coord.vec  - y.min) / (y.max - y.min) * n.dim))
  }
}

# get color
library(raster)
# devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

# read lcm
dpath <- 'downloads/lcm/GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/z001001.adf'
lcm.original <-  raster(dpath,layer=1)
# change ocean color
lcm.original@legend@colortable[128] <- '#006994'
# 
lcm.original@extent@xmin <- 109.504356
lcm.original@extent@xmax <- 157.215737
lcm.original@extent@ymin <- -44.318646
lcm.original@extent@ymax <- -8.139869

# pdf#####
pdf('figures/lcm with pasutre and tussock.pdf',width = 8,height = 8)
plot(lcm.original,asp=1)
# plot choosen sites

modis.df.tussock <- read.csv('cache/chosen sites.csv')
modis.df.tussock <- modis.df.tussock[seq(1,20,by=2),]

modis.df.pasture <- read.csv('cache/chosen pasture sites.csv')

# pasutre sites
points(x = modis.df.tussock$x,
       y = modis.df.tussock$y,
       pch=16,col='black',cex=1)
points(x = modis.df.pasture$x,
       y = modis.df.pasture$y,
       pch=16,col='red',cex=1)

legend(x=111,y=-36.5,legend = c('Chosen Tussock','Chosen Pasture'),
       pch=c(16),col=c('black','red'),bg='grey')

dev.off()
