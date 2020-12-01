# get ndvi
ndvi.df <- read.csv('downloads/ndvi_all.csv')
ndvi.df$Date <- as.Date(ndvi.df$system.time_start,'%b %d, %Y')

library(tidyr)
ndvi.df.long <-pivot_longer(ndvi.df,MGAR:YM,'Code',values_to = 'ndvi')
ndvi.df.long$NDVI.val <- as.numeric(gsub(',','',as.character(ndvi.df.long$ndvi))) * 0.0001

# get fapar
fapar.df.cw <- read.csv('downloads/fapar_cw.csv')
fapar.df.cw$Date <- as.Date(fapar.df.cw$system.time_start,'%b %d, %Y')

library(tidyr)
fapar.df.cw.long <-pivot_longer(fapar.df.cw,MGAR:IN17,'Code',values_to = 'fapar')
fapar.df.cw.long$fapar.val <- as.numeric(gsub(',','',as.character(fapar.df.cw.long$fapar))) * 0.01
# 
fapar.df.dn <- read.csv('downloads/fapar_dn.csv')
fapar.df.dn$Date <- as.Date(fapar.df.dn$system.time_start,'%b %d, %Y')

library(tidyr)
fapar.df.dn.long <-pivot_longer(fapar.df.dn,NG:YM,'Code',values_to = 'fapar')
fapar.df.dn.long$fapar.val <- as.numeric(gsub(',','',as.character(fapar.df.dn.long$fapar))) * 0.01

fapar.df.long <- rbind(fapar.df.dn.long,fapar.df.cw.long)

#get modis fractional cover  
watson.sites <- read.csv('watson_site.csv')
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



png('pair.gcc.fc.ndvi.fapar.png',width = 800,height = 800 )
pairs(gcc.fapar.fc.ndvi.df[,c('FC','GCCmean','NDVI.val','fapar.val')],upper.panel=NULL,
      panel=function(x,y){
        points(x,y)
        abline(lm(y~x), col='red',lwd=2,lty='dashed')
        text(x=quantile(x,probs = 0.99,na.rm = T),y=quantile(y,probs = 0.99,na.rm = T),
             labels = paste('R=',round((cor(x,y,use ='na.or.complete')),2)),col='red' )
      })
dev.off()
