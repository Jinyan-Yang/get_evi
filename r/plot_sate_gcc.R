# get ndvi
ndvi.df <- read.csv('downloads/ndvi_all.csv')
ndvi.df$Date <- as.Date(ndvi.df$system.time_start,'%b %d, %Y')

library(tidyr)
ndvi.df.long <-pivot_longer(ndvi.df,MGAR:YM,'Code',values_to = 'ndvi')
ndvi.df.long$NDVI.val <- as.numeric(gsub(',','',as.character(ndvi.df.long$ndvi))) * 0.0001

# get fapar
fapar.df.cw <- read.csv('downloads/fapar_cw.csv')
fapar.df.cw$Date <- as.Date(fapar.df.cw$system.time_start,'%b %d, %Y')

# 
fapar.df.cw.long <-pivot_longer(fapar.df.cw,MGAR:IN17,'Code',values_to = 'fapar')
fapar.df.cw.long$fapar.val <- as.numeric(gsub(',','',as.character(fapar.df.cw.long$fapar))) * 0.01
# 
fapar.df.dn <- read.csv('downloads/fapar_dn.csv')
fapar.df.dn$Date <- as.Date(fapar.df.dn$system.time_start,'%b %d, %Y')

fapar.df.dn.long <-pivot_longer(fapar.df.dn,NG:YM,'Code',values_to = 'fapar')
fapar.df.dn.long$fapar.val <- as.numeric(gsub(',','',as.character(fapar.df.dn.long$fapar))) * 0.01

fapar.df.long <- rbind(fapar.df.dn.long,fapar.df.cw.long)

#get modis fractional cover  
watson.sites <- read.csv('watson_site_new.csv')
sites.vec <- c('NG','MP','QP','YM',as.character(watson.sites$code))

out.ls <- list()

for(i in seq_along(sites.vec)){
  
  site.nm <- sites.vec[i]
  file.vec <- list.files('cache/',paste0('^',site.nm,'20'))
  
  fn.vec <- paste0('cache/',file.vec)
  tmp.ls <- list()
  for(j in seq_along(fn.vec)){
    print(fn.vec)
    tmp.df <- readRDS(fn.vec[j])
    tmp.ls[[j]] <- tmp.df[,c('Date','site','FC')]
    
    names(tmp.ls[[j]]) <- c('Date','Code','FC')
  }
  
  out.ls[[i]] <- do.call(rbind,tmp.ls)
  
}

fc.df <- do.call(rbind,out.ls)


#get modis fractional cover  CW
cw.gcc.df <- read.csv('downloads/GrasslandGCC_12Sites_Fulldataset.csv')
cw.gcc.df$Date <- as.Date(as.character(cw.gcc.df$daterep),'%d/%m/%Y')

library(doBy)
cw.gcc.df.cln <- summaryBy(GCCmean~Date + Location + Code + PS + Native,
                           data = cw.gcc.df,FUN=mean,na.rm=TRUE,
                           keep.names = TRUE)


# merge all
gcc.fapar.fc.ndvi.df <- Reduce(function(x,y) merge(x = x, y = y, all=T), 
       list(fapar.df.long, fc.df, ndvi.df.long,cw.gcc.df.cln))

saveRDS(gcc.fapar.fc.ndvi.df,'gcc.fc.ndvi.fapar.rds')


# 
png('pair.gcc.fc.ndvi.fapar.png',width = 800,height = 800 )

pairs(gcc.fapar.fc.ndvi.df[,c('FC','GCCmean','NDVI.val','fapar.val')],upper.panel=NULL,
      panel=function(x,y){
        points(x,y)
        abline(lm(y~x), col='red',lwd=2,lty='dashed')
        text(x=quantile(x,probs = 0.99,na.rm = T),y=quantile(y,probs = 0.99,na.rm = T),
             labels = paste('R=',round((cor(x,y,use ='na.or.complete')),2)),col='red' )
      })
dev.off()

# get a pallete
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
# 

palette(col.df$auLandscape)
pdf('time Series And Pairs By Site.pdf',width = 8,height = 8*.618)
par(mar=c(5,5,1,5))
gcc.met.cw.df <- readRDS('cache/gcc.met.cw.df.rds')
site.vec <- unique(gcc.met.cw.df$Code)
site.vec <- as.character(site.vec)

for (i in seq_along(site.vec)) {
  cw.sat.gcc.df <- gcc.fapar.fc.ndvi.df[gcc.fapar.fc.ndvi.df$Code == site.vec[i],]
  
  
  plot(GCCmean~Date,data = cw.sat.gcc.df,type='b',pch=16,col=2,#'blue',
       xlab='',ylab='Greenness',xlim=c(as.Date('2014-1-1'),
                                       as.Date('2016-1-1')),
       ylim=c(0.28,0.45)
       )
  title(site.vec[i])
  legend('topleft',legend = c('GCC','Fractional cover','NDVI','FAPAR','VWC'),
         pch=16,col=c(2,1,3,4,5))#col=c('red','coral','orange','blue'))
  if(sum(cw.sat.gcc.df$FC,na.rm=T)>0){
    par(new=T)
    plot(FC~Date,data = cw.sat.gcc.df,type='p',pch=16,col=1,#'red',
         ann=F,axes=F,xlim=c(as.Date('2014-1-1'),
                             as.Date('2016-1-1')))
  }
  if(sum(cw.sat.gcc.df$NDVI.val,na.rm=T)>0){
    par(new=T)
    plot(NDVI.val~Date,data = cw.sat.gcc.df,type='p',pch=16,col=3,#'coral',
             ann=F,axes=F,xlim=c(as.Date('2014-1-1'),
                                 as.Date('2016-1-1')))
  }
  
  if(sum(cw.sat.gcc.df$fapar.val,na.rm=T)>0){
    par(new=T)
    plot(fapar.val~Date,data = cw.sat.gcc.df,type='p',pch=16,col=4,#'orange',
         ann=F,axes=F,xlim=c(as.Date('2014-1-1'),
                             as.Date('2016-1-1')))
  }

  # rainfall
  fn.site <- paste0('cache/met.',site.vec[i],'20012016.rds')
  site.rain.df <- readRDS(fn.site)
  
  par(new=T)
  plot(Rain~Date,data = site.rain.df,type='s',col=5,#'lightskyblue',
       ann=F,axes=F,xlim=c(as.Date('2014-1-1'),
                           as.Date('2016-1-1')))
  
  y.max <- ceiling(max(site.rain.df$Rain,na.rm = T)/10)*10
  axis(4,at = seq(0,y.max,length.out = 5),labels = seq(0,y.max,length.out = 5))
  mtext('Rainfall (mm/d)',side =4, line =3)
  
  try(pairs(cw.sat.gcc.df[,c('FC','GCCmean','NDVI.val','fapar.val')],upper.panel=NULL,
        panel=function(x,y){
          if(sum(x,na.rm=T)>0 & sum(y,na.rm=T)>0){
            points(x,y)
            abline(lm(y~x), col='red',lwd=2,lty='dashed')
            text(x=quantile(x,probs = 0.99,na.rm = T),y=quantile(y,probs = 0.99,na.rm = T),
                 labels = paste('R=',round((cor(x,y,use ='na.or.complete')),2)),col='red' )
            
          }else{
           plot(1,main='NA')
          }
        }))
  title(site.vec[i])
}


dev.off()
