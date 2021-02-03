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

range(tmp.df.19$map,na.rm=T)

levels(tmp.df.19$map.level)
