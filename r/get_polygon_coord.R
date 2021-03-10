tossuck.coord.df <- read.csv('cache/chosen sites.csv')
pasutre.coord.df <- read.csv('cache/chosen pasture sites.csv')

get.polygon.func <- function(lon,lat,step.size = 0.02){
  
  out.vec <- c(lon,lat,lon+step.size,lat,
               lon+step.size,lat+step.size,lon,lat+step.size,
               lon,lat)
  # return(paste0(out.vec,','))
  return(paste(paste0(out.vec,','), collapse = ''))
}

tossuck.coord.df$poly <- NA

for (i in seq_along(tossuck.coord.df$x)) {
  tossuck.coord.df$poly[i] <- get.polygon.func(tossuck.coord.df$x[i],tossuck.coord.df$y[i])
}


pasutre.coord.df$poly <- NA

for (i in seq_along(pasutre.coord.df$x)) {
  pasutre.coord.df$poly[i] <- get.polygon.func(pasutre.coord.df$x[i],pasutre.coord.df$y[i])
}

write.csv(tossuck.coord.df,'cache/chosen sites.csv',row.names = F)
write.csv(pasutre.coord.df,'cache/chosen pasture sites.csv',row.names = F)
