# DRL_AUTO_250HI_SOIL_R_20190131.dat
library(HIEv)
library(doBy)
library(lubridate)
# set token for hiev
if(file.exists("c:/hiev/token.txt")){
  setToken()
}else{
  stop(c("Token need to be in c:/hiev/token.txt"))
}

#prepare the folders
if(!dir.exists("downloads"))dir.create("downloads")

# 
ng.swc.df <- downloadTOA5('DRL_AUTO_450LO_SOIL_R',topath = 'downloads/450lo')
# cb.swc.df <- downloadTOA5('DRL_AUTO_350LO_SOIL_R',topath = 'download/350lo')
mp.swc.df <- downloadTOA5('DRL_AUTO_250HI_SOIL_R',topath = 'downloads/250h')
qp.swc.df <- downloadTOA5('DRL_AUTO_350HI_SOIL_R',topath = 'downloads/350h')

# 
today.date <- Sys.Date()

plot.rain.func <- function(ng.swc.df,site.nm,past.days = 365){
  
  # ng.swc.df$mon <- format(ng.swc.df$DateTime,'%Y-%m')
  ng.sum.df <- doBy::summaryBy(Rain_mm_Tot~Date,data = ng.swc.df,
                               FUN=sum,na.rm=T,keep.names = T)
  
  ng.sum.df <- ng.sum.df[year(ng.sum.df$Date) %in% c(2019,2020),]
  
  
  points(Rain_mm_Tot~Date,data = ng.sum.df[ng.sum.df$Date > today.date - past.days &
                                           ng.sum.df$Date < today.date, ],type='s',col='navy',
       xlab='',ylab='Rainfall(mm/d)')
  
  acu.rain.30 <- sum(ng.sum.df$Rain_mm_Tot[ng.sum.df$Date > today.date - past.days &
                                             ng.sum.df$Date < today.date])
  
  legend('topright',legend = site.nm,bty='n')
  
  # legend('topleft',legend = paste0('Rain.total = ',acu.rain.30),bty='n')
  
}
