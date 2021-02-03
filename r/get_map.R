# get a subset and aggregated lcm for tussock first 
if(!file.exists('cache/tusock_with_map.rds')){
  source('r/get_met_nc.R')
  # get the saved data.frame
  tmp.df <- readRDS('cache/croped_lcm_df.rds')
  
  # get the lc type needed
  tmp.df.19 <- tmp.df[tmp.df$aus5_1e_mvg == 19,]
  # tmp.df.21 <- tmp.df[tmp.df$aus5_1e_mvg == 21,]
  
  # read map
  tmp.df.19$map <- get.map.coord.func(tmp.df.19$y,tmp.df.19$x)
  
  tmp.df.19 <- tmp.df.19[!is.na(tmp.df.19$map),]
  
  rain.cut <- c((0:10)*100,2000)
  
  tmp.df.19$map.level <- cut(tmp.df.19$map,breaks = rain.cut,labels = paste0('<',rain.cut[-1]))
  
  saveRDS(tmp.df.19,'cache/tusock_with_map.rds')
}else{
  tussock.coords.df <- readRDS('cache/tusock_with_map.rds')
}

# sample for rainfall gradient
tussock.coords.df <- tussock.coords.df[tussock.coords.df$y> -35 & tussock.coords.df$y< -25,]
tmp.ls <- split(tussock.coords.df,tussock.coords.df$map.level)

rain.level.vec <- levels(tussock.coords.df$map.level)
for (i in seq_along(rain.level.vec)) {
  set.seed(1935)
  row.chosen <- sample(nrow(tmp.ls[[i]]),2)
  
  tmp.ls[[i]] <- tmp.ls[[i]][row.chosen,]
  
}

tussock.sample.df <- do.call(rbind,tmp.ls)
tussock.sample.df <- tussock.sample.df[tussock.sample.df$map <1000,]
# tussock.sample.df.tmp <- tussock.sample.df
# save chosen sites
write.csv(tussock.sample.df,'chosen sites.csv',row.names = F)
