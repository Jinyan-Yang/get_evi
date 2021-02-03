library(ncdf4)
library(ncdf)

nc_data <- nc_open('PHAC_forcing_d.nc')

year.vec <- ncvar_get(nc_data, "YEAR")

doy.vec <- ncvar_get(nc_data, "DOY")

rain.vec <- ncvar_get(nc_data, "Rainf")


met.phase.df <- data.frame(year = ncvar_get(nc_data, "YEAR"),
                           doy = ncvar_get(nc_data, "DOY"),
                           rain = ncvar_get(nc_data, "Rainf"),
                           tair = ncvar_get(nc_data, "Tair"),
                           par = ncvar_get(nc_data, "PAR"),
                           # rh = ncvar_get(nc_data, "Rainf"),
                           vpd = ncvar_get(nc_data, "VPD"),
                           wind = ncvar_get(nc_data, "Wind")
                          )
met.phase.df$date <- as.Date(met.phase.df$doy, origin = as.Date(paste0(met.phase.df$year,'-01-01')))
met.phase.df$tmax <- met.phase.df$tair + 5
met.phase.df$tmin <- met.phase.df$tair - 5

