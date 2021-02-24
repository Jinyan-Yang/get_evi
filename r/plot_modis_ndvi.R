devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

modis.ndvi.met.df <- readRDS('C:/repo/PhenoMods/cache/modis/modis.ndvi.met.rds')

site.vec <- unique(modis.ndvi.met.df$Species)

site.vec <- site.vec[c(4,5,6,7,8,9,10,1,2,3,11,13,14:20,12)]


# 
pdf('figures/modis ndvi.pdf',width = 8*2, height = 8*2*.618)

par(mar=c(4,5,1,4),mfrow=c(5,2))

for(i in seq_along(site.vec)){
  
  tmp.df <- modis.ndvi.met.df[modis.ndvi.met.df$Species == site.vec[i],]
  
  plot(Rain~Date,data = tmp.df,type='s',col=col.df$auLandscape[5],yaxt='n',
       xlab='',ylab='',main=site.vec[i],ylim=c(0,100))
  axis(4,at = seq(0,100,by=25),labels = seq(0,100,by=25))
  mtext('Rainfall (mm/d)',side=4,line=2.5,col=col.df$auLandscape[5])
  # 
  tmp.df.na.m <- tmp.df[!is.na(tmp.df$ndvi.val),]
  par(new=T)
  plot(ndvi.val~Date,data = tmp.df.na.m,ylim=c(0,1),xlim=range(tmp.df$Date),
       type='b',lwd=2,pch=16,col = col.df$auLandscape[2],
       ann=F,axes=F)
  axis(2,at = seq(0,1,by=.25),labels = seq(0,1,by=.25))
  mtext('NDVI',side=2,line=2.5,col=col.df$auLandscape[2])
  
  rain.mean <- round(sum(tmp.df$Rain,na.rm=T)/10)
  
  legend('topleft',legend = paste0('MAP during the period = ',rain.mean, ' mm/yr'),bty='n')
  
}

dev.off()

