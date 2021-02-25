library(raster)
# read in lcm
dpath <- 'downloads/lcm/GRID_NVIS6_0_AUST_EXT_MVG/aus6_0e_mvg/z001001.adf'
lcm.original <-  raster(dpath,layer=1)
# cut for east Au to reduce run time
cut.size <- aea2lonlat.fun(data.frame(x =c(140,158),y=-c(36,10)),input.is.aea = F)
e <- extent(cut.size$x[1], cut.size$x[2], cut.size$y[1], cut.size$y[2])

rc <- crop(lcm.original, e)
# aggreagte over 10*10 pixel area (1km * 1km )
rc.agg <- raster::aggregate(rc,10,expand=F,fun = function(x,na.rm=FALSE){
  i=1
  on.exit(return(i))
  if(!is.na(sum(x == 19))){
    if(sum(x == 19) >= 90){
      i=19
    }
  }

})

# convert the raster to a df with only coords
lcm.eastAU.df <- as.data.frame(rc.agg,xy=T)
names(lcm.eastAU.df) <- c('x','y','type')
tmp.df.coord <- aea2lonlat.fun(lcm.eastAU.df[,c('x','y')],input.is.aea=TRUE)
lcm.eastAU.df.gps <- cbind(lcm.eastAU.df[,'type'],tmp.df.coord)
names(lcm.eastAU.df.gps) <- c('type','x','y')
saveRDS(lcm.eastAU.df.gps,'cache/croped_lcm_df.rds')
