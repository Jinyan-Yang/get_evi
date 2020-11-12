# 

plot.evi.fc.func <- function(ym.modis.df,ym.18.df,site.nm){
  # plot(evi~Date,data = ym.18.df,ylim=c(0,1),type='b',lwd = 3,
  #      ylab = 'GCC / Live plant cover',pch=16,xlab='',xaxt='n')
  
  plot((FC/100)~Date,data = ym.modis.df,ylim=c(0,1),type='b',lwd = 3,
       ylab = 'EVI / Live plant cover',pch=16,xlab='',xaxt='n')
  
  points(evi~Date,data = ym.18.df,type='b',lwd=3,col='grey',pch=16)
  # 
  date.range = range(ym.modis.df$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)
  # mtext('2018',side = 1,adj=0,line = 3)
  # mtext('2019',side = 1,adj=0.5,line = 3)
  yr.vec <- unique(year(ym.modis.df$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  # 
  legend('topleft',legend=site.nm,bty='n')
  legend('topright',legend=c('EVI','FC'),lty=1,col=c('grey','black'),bty='n')
}
# 

pdf('compare.evi.fc.pdf',width = 6,height = 6*.618)
watson.sites <- read.csv('watson_site.csv')
site.vec <- unique(watson.sites$code)
for(i in seq_along(site.vec)){
 
  
  # if(class(cw.tmp.df) != 'try-error'){
    # 
    fn1 <- paste0('cache/',site.vec[i],2014,'.rds')
    tmp.df1 <- readRDS(fn1)
    
    fn2 <- paste0('cache/',site.vec[i],2015,'.rds')
    tmp.df2 <- readRDS(fn2)
    
    tmp.df <- rbind(tmp.df1,tmp.df2)
    
    # 
    fn1.evi <- paste0('cache/evi.',site.vec[i],'20012016','.rds')
    tmp.df.evi <- readRDS(fn1.evi)
    
    
    # tmp.df.evi <- rbind(tmp.df1.evi,tmp.df2.evi)
    
    
    plot.evi.fc.func(tmp.df,tmp.df.evi,site.vec[i])
  # }
  
  
}

dev.off()