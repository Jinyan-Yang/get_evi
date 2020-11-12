library(raster)
library(ncdf4)
library(gdalUtils)
library(R.utils)
library(lubridate)
source('r/reprojection_func.R')

# 
plot.tile.func <- function(tile.nm,time.of.year){

  nc_data <- nc_open(tile.nm)
  # # print(nc_data)

  # reproject modis data
  lon.vec <- (ncvar_get(nc_data, "x")) #* km2degree
  lat.vec <- (ncvar_get(nc_data, "y")) #* km2degree

  DF <- data.frame(x = lon.vec,y = lat.vec)
  coords.repro.df <- repro.modis.fun(DF)
  
  time.vec <- ncvar_get(nc_data, "time")
  class(time.vec) = c('POSIXct')
  
  
  # range(lon.vec)
  # range(lat.vec)
  
  # live cover
  fc <- ncvar_get(nc_data, "phot_veg")
  # dead cover
  # dc <- ncvar_get(nc_data, "nphot_veg")
  
  data.cln <- fc[,,time.of.year]
  data.cln[data.cln == 255] <- NA
  data.cln[data.cln > 100] <- 100

  plot.rs <- raster(t(data.cln)
                    ,xmn = min(lon.vec),xmx =  max(lon.vec),
                    ymn= min(lat.vec),ymx =  max(lat.vec)
                    )
  col.fun <- colorRampPalette(c('sandybrown','forestgreen'))

  image(plot.rs,breaks= seq(0,100,10),col=col.fun(10),ann=F,axes=F)
  # 
  # raster::plot(plot.rs,legend.only=TRUE,col = col.fun(10),horizontal = F,breaks = seq(0,100,10),
  #              legend.args=list(text='Plant Cover (%)', side=3, font=2, line=1, cex=0.8))
  # 
  title(as.Date(time.vec[time.of.year]))
  
  # x.dist= distm(c(146.641889,-31.645194),c(0,-31.645194),fun = distGeo)[1]
  # y.dist=-distm(c(146.641889,-31.645194),c(146.641889,0),fun = distGeo)[1]

  # points(y= -31.645194,x=146.641889,pch='N',col='red')
  # points(y= -26.577250,x=144.619028,pch='Q',col='red')
  # points(y= -33.613540,x=150.738132,pch='Y',col='red')
  DN.site.info.df <- data.frame(site = c('NG','MP','QP','YM'),
                                lat = -c(31.645194,29.607250,26.577250,33.613540 ),
                                lon = c(146.641889,141.712600,144.619028,150.738132))
  
  DF <- data.frame(x = DN.site.info.df$lon,y = DN.site.info.df$lat)
  sinu.df <- coord2sinu.fun(DF)
  
  points(y= sinu.df$y[1],x=sinu.df$x[1],pch='N',col='red')
  points(y= sinu.df$y[2],x=sinu.df$x[2],pch='M',col='red')
  points(y= sinu.df$y[3],x=sinu.df$x[3],pch='P',col='red')
  points(y= sinu.df$y[4],x=sinu.df$x[4],pch='Y',col='red')
  
}

pdf('modis.plant.cover.au.2020.pdf',width = 8,height = 8)
for (i in 1:34){
  
  par(mfrow=c(3,4))
  par(mar=c(0,0,0,0))
  
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h28v10.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h29v10.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h30v10.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h31v10.2020.006.nc',i)
  
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h28v11.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h29v11.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h30v11.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h31v11.2020.006.nc',i)
  
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h28v12.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h29v12.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h30v12.2020.006.nc',i)
  plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h31v12.2020.006.nc',i)
  
  # plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h31v13.2020.006.nc',i)
  # points(y= -26.577250,x=144.619028,pch='Q',col='red')


# 
# plot.tile.func(tile.nm='downloads/modisFC/FC.v310.MCD43A4.h27v12.2020.006.nc',i)
# x.dist= distm(c(150.738132,-33.613540),c(0,33.613540),fun = distHaversine)[1]
# y.dist=-distm(c(150.738132,-33.613540),c(150.738132,0),fun = distHaversine)[1]
# 
# points(y= -33.613*1e5,x=150.7300*1e5,pch='N',col='red')
}
dev.off()
