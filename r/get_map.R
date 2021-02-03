source('r/get_met_nc.R')
# get the saved data.frame
tmp.df <- readRDS('cache/croped_lcm_df.rds')

# get the lc type needed
tmp.df.19 <- tmp.df[tmp.df$aus5_1e_mvg == 19,]
# tmp.df.21 <- tmp.df[tmp.df$aus5_1e_mvg == 21,]

# read map
tmp.df.19$map <- get.map.coord.func(tmp.df.19$y,tmp.df.19$x)
saveRDS(tmp.df.19,'tusock_with_map.rds')