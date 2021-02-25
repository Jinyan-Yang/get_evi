# get a subset and aggregated lcm for tussock first 
if(!file.exists('cache/tusock_with_map.rds')){
  source('r/get_met_nc.R')
  # get the saved data.frame
  tmp.df <- readRDS('cache/croped_lcm_df.rds')
  
  # get the lc type needed
  tmp.df.19 <- tmp.df[tmp.df$type == 19,]
  # tmp.df.21 <- tmp.df[tmp.df$aus5_1e_mvg == 21,]
  
  # read map
  tmp.df.19$map <- get.map.coord.wc.func(tmp.df.19$y,tmp.df.19$x)
  
  tmp.df.19 <- tmp.df.19[!is.na(tmp.df.19$map),]
  
  rain.cut <- c((1:10)*100,2000)
  
  tmp.df.19$map.level <- cut(tmp.df.19$map,breaks = rain.cut,labels = paste0('<',rain.cut[-1]))
  tmp.df.19$map.level <- droplevels(tmp.df.19$map.level)
  saveRDS(tmp.df.19,'cache/tusock_with_map.rds')
  tussock.coords.df <- readRDS('cache/tusock_with_map.rds')
}else{
  tussock.coords.df <- readRDS('cache/tusock_with_map.rds')
}

# sample for rainfall gradient
tussock.coords.df <- tussock.coords.df[tussock.coords.df$y> -35 & tussock.coords.df$y< -25,]
tmp.ls <- split(tussock.coords.df,tussock.coords.df$map.level)
# hist(tussock.coords.df$map)
# min(tussock.coords.df$map)

rain.level.vec <- levels(tussock.coords.df$map.level)
for (i in seq_along(rain.level.vec)) {
  set.seed(1935)
  row.chosen <- sample(nrow(tmp.ls[[i]]),1)
  
  tmp.ls[[i]] <- tmp.ls[[i]][row.chosen,]
  
}

tussock.sample.df <- do.call(rbind,tmp.ls)
x.vec <- paste0(tussock.sample.df$x,',',tussock.sample.df$y)
tussock.sample.df <- tussock.sample.df[tussock.sample.df$map <2000,]
# tussock.sample.df.tmp <- tussock.sample.df
# save chosen sites
write.csv(tussock.sample.df,'cache/chosen sites.csv',row.names = F)



# get MAP for pasture#########################################################
if(!file.exists('cache/pasutre_with_map.rds')){
  source('r/get_met_nc.R')
  # get the saved data.frame
  tmp.df.pasture <- readRDS('cache/croped_pasture_df.rds')
  
  # read map
  tmp.df.pasture$map <- get.map.coord.func(tmp.df.pasture$y,tmp.df.pasture$x)
  
  tmp.df.pasture <- tmp.df.pasture[!is.na(tmp.df.pasture$map),]
  
  rain.cut <- c((0:12)*100,2000)
  
  tmp.df.pasture$map.level <- cut(tmp.df.pasture$map,breaks = rain.cut,labels = paste0('<',rain.cut[-1]))
  tmp.df.pasture <- tmp.df.pasture[!is.na(tmp.df.pasture$map),]
  saveRDS(tmp.df.pasture,'cache/pasture_with_map.rds')
}else{
  pasture.coords.df <- readRDS('cache/pasture_with_map.rds')
}

# sample for rainfall gradient
pasture.coords.df <- pasture.coords.df[pasture.coords.df$map <1200,]
tmp.ls <- split(pasture.coords.df,pasture.coords.df$map.level)

pasture.coords.df$map.level <- droplevels(pasture.coords.df$map.level)

rain.level.vec <- levels(pasture.coords.df$map.level)
for (i in seq_along(rain.level.vec)) {
  set.seed(1935)
  row.chosen <- sample(nrow(tmp.ls[[i]]),1)
  
  tmp.ls[[i]] <- tmp.ls[[i]][row.chosen,]
  
}

pasture.coords.df.shooen <- do.call(rbind,tmp.ls)

# tussock.sample.df.tmp <- tussock.sample.df
# save chosen sites
write.csv(pasture.coords.df.shooen,'cache/chosen pasture sites.csv',row.names = F)
