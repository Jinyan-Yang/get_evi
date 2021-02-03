cw.gcc.df <- read.csv('downloads/GrasslandGCC_12Sites_Fulldataset.csv')
cw.gcc.df$Date <- as.Date(as.character(cw.gcc.df$daterep),'%d/%m/%Y')

library(doBy)
cw.gcc.df.cln <- summaryBy(GCCmean~Date + Location + Code + PS + Native,
                           data = cw.gcc.df,FUN=mean,na.rm=TRUE,
                           keep.names = TRUE)


site.vec <- unique(cw.gcc.df.cln$Code)
site.vec <- as.character(site.vec)

pdf('watson.norm.pdf',width = 8,height = 4*3*0.618)

par(mfrow = c(3,2))

for (i in seq_along(site.vec)) {
  
  # plot(GCCmean~Date ,data = cw.gcc.df.cln[cw.gcc.df.cln$Location == site.vec[i],],
       # xaxt='n',pch=16,col='darkseagreen',xlab='')
  
  tmp.df <- readRDS(sprintf('cache/evi.%s20012016.rds',site.vec[i]))
  
  plot.df <- tmp.df[tmp.df$year %in% 2014:2015,]
  
  plot.df$evi.norm <- (plot.df$evi - min(plot.df$evi,na.rm=T)) / 
    (max(plot.df$evi,na.rm=T) - min(plot.df$evi,na.rm=T))
  
  plot(evi.norm~Date,data = plot.df,lwd=4,col='grey',type='l',
       ylab='Vegetation index',xlab='',
       xaxt='n')
  # par(new=T)
  # plot(GCCmean~Date,cw.gcc.df.cln[cw.gcc.df.cln$Code == watson.sites$code[i],],pch=16,col='darkseagreen',
  #      ann=F,axes=F,xlab="",ylab='')
  # 
  
  gcc.tmp.df <- cw.gcc.df.cln[cw.gcc.df.cln$Code == site.vec[i],]
  gcc.tmp.df$gcc.norm <- (gcc.tmp.df$GCCmean - min(gcc.tmp.df$GCCmean,na.rm=T) ) / 
    (max(gcc.tmp.df$GCCmean,na.rm=T) - min(gcc.tmp.df$GCCmean,na.rm=T))
  points(gcc.norm~Date,gcc.tmp.df,pch=16,col='darkseagreen')

  mon.vec <- seq(min(plot.df$Date),max(plot.df$Date),by='month')
  
  # library(lubridate)
  axis(1,at = mon.vec, labels = format(mon.vec,'%b'))

  mtext('2014',side=1,line=2.5,adj = 0)
  mtext('2015',side=1,line=2.5,adj = 0.5)
  legend('topright',legend = site.vec[i],bty='n')
  legend('bottomright',legend = c('GCC','EVI'),pch=16,col=c('darkseagreen','grey'),bty='n')
}

dev.off()

# 
pdf('watson.ori.pdf',width = 8,height = 4*3*0.618)

par(mfrow = c(3,2))

for (i in seq_along(site.vec)) {
  
  # plot(GCCmean~Date ,data = cw.gcc.df.cln[cw.gcc.df.cln$Location == site.vec[i],],
  # xaxt='n',pch=16,col='darkseagreen',xlab='')
  
  tmp.df <- readRDS(sprintf('cache/evi.%s20012016.rds',site.vec[i]))
  
  plot.df <- tmp.df[tmp.df$year %in% 2014:2015,]
  
  # plot.df$evi.norm <- (max(plot.df$evi,na.rm=T) - plot.df$evi) / 
  #   (max(plot.df$evi,na.rm=T) - min(plot.df$evi,na.rm=T))
  
  plot(evi~Date,data = plot.df,lwd=4,col='grey',type='l',
       ylab='Vegetation index',xlab='',
       xaxt='n',ylim=c(0.1,0.5))
  # par(new=T)
  # plot(GCCmean~Date,cw.gcc.df.cln[cw.gcc.df.cln$Code == watson.sites$code[i],],pch=16,col='darkseagreen',
  #      ann=F,axes=F,xlab="",ylab='')
  # 
  
  gcc.tmp.df <- cw.gcc.df.cln[cw.gcc.df.cln$Code == site.vec[i],]
  # gcc.tmp.df$gcc.norm <- (max(gcc.tmp.df$GCCmean,na.rm=T) - gcc.tmp.df$GCCmean) /
  #   (max(gcc.tmp.df$GCCmean,na.rm=T) - min(gcc.tmp.df$GCCmean,na.rm=T))
  points(GCCmean~Date,gcc.tmp.df,pch=16,col='darkseagreen')
  
  mon.vec <- seq(min(plot.df$Date),max(plot.df$Date),by='month')
  
  # library(lubridate)
  axis(1,at = mon.vec, labels = format(mon.vec,'%b'))
  
  mtext('2014',side=1,line=2.5,adj = 0)
  mtext('2015',side=1,line=2.5,adj = 0.5)
  legend('topright',legend = site.vec[i],bty='n')
  legend('bottomright',legend = c('GCC','EVI'),pch=16,col=c('darkseagreen','grey'),bty='n')
}

dev.off()
