library(ncdf4)
library(raster)
library(sf)
library(lattice)
library(RColorBrewer)

# precip: 1991-2020 ----
# prcp_all <- raster(here::here("Data",
#                               "Precip",
#                               "downloaded from NCEI monthly normals",
#                               "prcp-1991_2020-monthly-normals-v1.0.nc"))

dat_nc <- nc_open(here::here("Data",
                              "Precip",
                              "downloaded from NCEI monthly normals",
                              "prcp-1991_2020-monthly-normals-v1.0.nc"))

# get lat and long
lon <- ncvar_get(dat_nc, "lon")
dim(lon)  # 1385

lat <- ncvar_get(dat_nc, "lat")
dim(lat)  # 596

# get precip variables
annprcp_norm <- "annprcp_norm"  # annual precipitation normals
annprcp_mat <- ncvar_get(dat_nc, annprcp_norm)
dim(annprcp_mat) # 1385 x 596

mlyprcp_norm <- "mlyprcp_norm"
mlyprcp_array <- ncvar_get(dat_nc, mlyprcp_norm)
dim(mlyprcp_array) # 1385 x 596 x 12

# get fill values
fillvalueAnn <- ncatt_get(dat_nc, annprcp_norm, "_FillValue")
fillvalueMly <- ncatt_get(dat_nc, mlyprcp_norm, "_FillValue")

# get time
time <- ncvar_get(dat_nc, "time")
time
dim(time)

tunits <- ncatt_get(dat_nc,"time","units")
tunits

# close connection
nc_close(dat_nc)

# replace netCDF fill values with NA's
mlyprcp_array[mlyprcp_array == fillvalueMly$value] <- NA
annprcp_mat[annprcp_mat == fillvalueAnn$value] <- NA

# change it to inches
annprcp_mat <- annprcp_mat / 25.4
mlyprcp_array <- mlyprcp_array / 25.4

# make a plot of annual precip
# first make it a data frame
grid <- expand.grid(lon=lon, lat=lat)
levelplot(annprcp_mat ~ lon * lat, data=grid, pretty=T,
          main = "30-yr Normal Annual Precipitation")


# monthly plots
for(i in 1:12){
    slice <- mlyprcp_array[,,i]
    print(levelplot(slice ~ lon * lat, data=grid, pretty=T,
                    main = paste0("Monthly Precip: ", i)))
}


# # MSEP ----
# 
# msep <- st_read(here::here("Data",
#                            "Watershed Boundaries",
#                            "combined",
#                            "MSEP_Outline.shp"))
# msep <- transform(msep, st_crs(prcp_all))
# 
# # bounding box
# msep_bb <- st_bbox(msep)
# 
# # trim ----
# prcp_msepbb <- crop(prcp_all, msep_bb)
# prcp_ms <- mask(prcp_msepbb, msep)
# 
# # plot it
# plot(prcp_ms)
# # write it out
# writeRaster(prcp_ms, here::here("Data",
#                                 "Precip",
#                                 "trimmed-NCEI",
#                                 "prcp-1991_2020-msep.nc"))
# 
