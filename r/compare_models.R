evi.ym.df <- readRDS('ym20012016.rds')
huf.df <- read.csv('outPutHufken.csv')
met.df <- read.csv('met.csv')

sgs.df <- read.csv('sgs_ym.csv',skip=2)
sgs.df$Date <- as.Date(as.character(sgs.df$Date),'%d/%m/%Y')

par(mar=c(3,5,1,1))
with(sgs.df,plot(LAI.tot~Date,xlab='',ylab = expression(LAI~(m^2~m^-2))))


pdf('evi_model.pdf',width = 8,height = 8*0.618)
par(mar=c(3,5,1,1))
par(bg = rgb(248/255,248/255,248/255))
plot(evi~Date,data = evi.ym.df[evi.ym.df$year %in% c('2013','2014'),],type='b',
     lwd=3,pch=16,col='grey80',
     xlim=c(as.Date('2013-1-1'),as.Date('2014-12-31')),
     xlab='',ylab='EVI')
par(new=T)
plot(huf.df$lai,type='s',ann=F,axes=F)
par(new=T)
plot(met.df$rain[(met.df$X.year) %in% c(2013,2014)],type='s',ann=F,axes=F,col='lightskyblue')
legend('topright',legend = c('MODIS','PTM','Rainfall'),lty='solid',lwd=2,col=c('grey','black','lightskyblue'),bty='n')

# 
plot(evi~Date,data = evi.ym.df[evi.ym.df$year %in% c('2013','2014'),],type='b',
     lwd=3,pch=16,col='grey80',
     xlim=c(as.Date('2013-1-1'),as.Date('2014-12-31')),
     xlab='',ylab='EVI')

par(new=T)
plot(met.df$rain[(met.df$X.year) %in% c(2013,2014)],type='s',ann=F,axes=F,col='lightskyblue')
legend('topright',legend = c('MODIS','Rainfall'),lty='solid',lwd=2,col=c('grey','lightskyblue'),bty='n')

# 
plot(evi~Date,data = evi.ym.df[evi.ym.df$year %in% c('2013','2014'),],type='b',
     lwd=3,pch=16,col='grey80',
     xlim=c(as.Date('2013-1-1'),as.Date('2014-12-31')),
     xlab='',ylab='EVI')
par(new=T)
with(sgs.df,plot(LAI.tot~Date,type='s',ann=F,axes=F))
legend('top',legend = c('MODIS','SGS'),lty='solid',lwd=2,,col=c('grey','black'),bty='n')

dev.off()
