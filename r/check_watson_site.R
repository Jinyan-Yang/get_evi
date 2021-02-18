watson.sites <- read.csv('watson_site.csv')

# 
tmp.df <- readRDS(sprintf('cache/evi.%s20012016.rds','TURA'))

plot(evi~Date,data = tmp.df[tmp.df$year == 2014,],pch=16,col='grey',type='b')

points(GCCmean~Date,cw.gcc.df.cln[cw.gcc.df.cln$Code == 'TURA',],pch=16,col='darkseagreen')


# 
tmp.df <- readRDS(sprintf('cache/evi.%s20012016.rds','MPON'))

plot(evi~Date,data = tmp.df[tmp.df$year == 2014,],pch=16,col='grey',type='b')

points(GCCmean~Date,cw.gcc.df.cln[cw.gcc.df.cln$Code == 'MPON',],pch=16,col='darkseagreen')



# plot the sites location
watson.sites <- read.csv('watson_site.csv')
# get ym
lat <- -33.610412
lon <- 150.73394


# get qp

lat <- -26.577250
lon <- 144.619028


# ng

lat <- -31.645194
lon <- 146.641889


# # jp
# -36.878906, 141.696658
# # fc
# -36.865144, 147.270622
# # mp
# -29.719373, 141.895120
dn.sites <- data.frame(code = c('ym','qp','ng','jp','fc','mp'),
                       lat = c(-33.610412,-26.577250,-31.645194,-36.878906,-36.865144,-29.719373),
                       lon = c(150.73394,144.619028,146.641889,141.696658,147.270622,141.895120),
                       type=NA)

library("rnaturalearth")
library("rnaturalearthdata")



world <- ne_countries(scale = "medium", returnclass = "sf")

library("ggspatial")

ggplot(data = world) +
  # geom_text(data= dn.sites,aes(y=lon, x=lat, label=code),
  #           color = "lightseagreen", fontface = "bold", check_overlap = TRUE)+
  geom_sf() +
  annotate(geom = 'text', y = dn.sites$lat, x = dn.sites$lon, label = dn.sites$code,
           fontface = "italic", color = "lightseagreen", size = 3)+
  annotate(geom = 'text', y = watson.sites$lat, x = watson.sites$lon, label = (watson.sites$code),
           fontface = "italic", color = "red", size = 2)+
  coord_sf(xlim = c(140, 152), ylim = c(-39, -25), expand = FALSE)




