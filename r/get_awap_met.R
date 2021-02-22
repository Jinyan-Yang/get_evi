# get gridded climate from bom site
# http://www.bom.gov.au/jsp/awap/rain/index.jsp

# 
# url1 <- "http://www.bom.gov.au/web03/ncc/www/awap/rainfall/totals/daily/grid/0.05/history/nat/"
# ### file names
# 
# day.list <- seq.Date(as.Date("2020/01/01"),
#                      as.Date("2020/01/31"),
#                      by="day")
# 
# day.list <- gsub("-", "", day.list)
# 
# destDir <- 'downloads/precipitation'
# 
# curl_download_withtest <- function(filename, url,...){
#   if(!file.exists(filename)){
#     download.file(url, filename,"curl",quiet = FALSE,...)
#   } else {
#     message("Skipped ",filename, "; already exists.")
#   }
# }
# ### download command
# zip.vec <- c()
# for (i in seq_along(day.list)) {
#   # download.file(url=paste0(url1, i, i, ".grid.Z"),
#   #               destfile=paste0(destDir, "/", i, i, ".grid.Z"))
#   
#   url.tmp <- paste0(url1, day.list[i], day.list[i], ".grid.Z")
#   zip.vec[i] <- paste0(destDir, "/",  day.list[i],  day.list[i], ".grid.Z") 
#   
#   curl_download_withtest( zip.vec[i],url.tmp)
#   
#   # Sys.sleep(2)
#   # read.table('downloads/precipitation/2020010120200101.grid.Z')
# 
# }
# 
# source('r/functions_awap.R')
# 
# Decompress7Zip(zipFileName=sprintf("G:\\repo\\get_evi\\downloads\\precipitation\\2019022120190221.grid.Z"),
#                outputDirectory="G:\\repo\\get_evi\\downloads\\precipitation\\unziped",delete=FALSE)


get_awap_data('2010-1-1','2020-12-31','maxave','downloads/dailyTmax')

get_awap_data('2010-1-1','2020-12-31','minave','downloads/dailyTmin')

get_awap_data('2010-1-1','2020-12-31','totals','downloads/precipitation')

get_awap_data('2010-1-1','2020-12-31','vprph09','downloads/dailyVP09')

get_awap_data('2010-1-1','2020-12-31','vprph15','downloads/dailyVP15')

get_awap_data('2010-1-1','2020-12-31','solarave','downloads/dailyPAR')

# daa reading
library(raster)
tmp <- raster('downloads/precipitation/unziped/2019022120190221.grid')

extract(tmp,c(140,-35))

sp <- SpatialPoints(cbind(140,-35))
rainfall.vec <- extract(tmp, sp, method='bilinear')
