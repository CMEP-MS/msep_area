library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)

nc_in <- here::here("Bathymetry", 
                     "mississippi_sound_g170_30m.nc")
dname <- "tmp"  # note: tmp means temperature (not temporary)

dat_nc <- nc_open(nc_in)
print(dat_nc)

# get lat and long
easting <- ncvar_get(dat_nc, "x")
dim(easting)

northing <- ncvar_get(dat_nc, "y")
dim(northing)

# get variables
Band1 <- ncvar_get(dat_nc, "Band1")
dlname <- ncatt_get(dat_nc, "Band1", "long_name")
dunits <- ncatt_get(dat_nc, "Band1", "units")
fillvalue <- ncatt_get(dat_nc, "Band1", "_FillValue")
dim(Band1)


# close the file
nc_close(dat_nc)

# replace netCDF fill values with NA's
Band1[Band1==fillvalue$value] <- NA


tmp_small <- Band1[, 100:300]
image(easting, northing, Band1, col = rev(brewer.pal(10, "RdBu")))


# levelplot of the slice
grid <- expand.grid(easting = easting, northing = northing)
# cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(Band1 ~ easting * northing, data=grid, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


# create a data frame
coords <- as.matrix(expand.grid(easting, northing))
# vector of values
band1_vec <- as.vector(Band1)

# get into df format
tmp_df01 <- data.frame(cbind(coords, band1_vec))
names(tmp_df01) <- c("easting", "northing", "band1")

tmp_df02 <- na.omit(tmp_df01)

# mean depth
mean(tmp_df02$band1)
# -2.95

# range of depths
hist(tmp_df02$band1, breaks = 50)

# quantiles of depth
quantile(tmp_df02$band1, probs = c(0.05, 0.10, 0.25, 0.5, 0.75, 0.9, 0.95))

# most of the sound is less than 5m deep; how much is deeper?
sum(tmp_df02$band1 < -5) / nrow(tmp_df02)
# 7%

levelplot(band1 ~ easting * northing, data=tmp_df02, cuts = 10, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

saveRDS(tmp_df02, here::here("Bathymetry", "Bathy_readings.RDS"),
        compress = "xz")
