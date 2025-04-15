library(ncdf4)
library(raster)
library(rasterVis)
library(terra)
library(sf)
library(USAboundaries)
library(lattice)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)

# get MS boundary from USAboundaries package
ms <- us_states(states = "Mississippi")

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


# Subset to MS ----
# 
# annual ----
# Transpose data because raster expects matrix in [rows = y, cols = x]
annprcp_mat_t <- t(annprcp_mat)

# Create raster
r_ann <- raster::raster(annprcp_mat_t, 
                        xmn = min(lon), xmx = max(lon), 
                        ymn = min(lat), ymx = max(lat),
                        crs = CRS("+proj=longlat +datum=WGS84"))

# Crop and mask the raster using the Mississippi polygon
r_ann_ms <- raster::mask(raster::crop(r_ann, ms), ms)

plot(r_ann_ms, main = "Annual Precipitation (Mississippi)")


# monthly ----
# Transpose the data: [lon, lat, time] -> [lat, lon, time]
mlyprcp_reordered <- aperm(mlyprcp_array, c(2, 1, 3))  # Now: [lat, lon, time]

# Create the RasterBrick
r_brick <- brick(mlyprcp_reordered,
                 xmn = min(lon), xmx = max(lon),
                 ymn = min(lat), ymx = max(lat),
                 crs = CRS("+proj=longlat +datum=WGS84"))
# Crop and mask all 12 layers at once
r_brick_ms <- mask(crop(r_brick, ms), ms)

names(r_brick_ms) <- month.abb

# Plot January, for example
plot(r_brick_ms[[1]], main = "January Precip (Mississippi)")

# Or all layers
plot(r_brick_ms)

# plotting entire raster brick ----
# set up uniform color palette
my_colors <- colorRampPalette(brewer.pal(9, "GnBu"))(11)  # Smooth gradient
global_min <- min(values(r_brick_ms), na.rm = TRUE)
global_max <- max(values(r_brick_ms), na.rm = TRUE)

plot(r_brick_ms,
     col = my_colors,
     zlim = c(global_min, global_max)  # This locks the color scale NO IT DOES NOT
     )

crs_backup <- crs(r_brick_ms)
crs(r_brick_ms) <- NA

levelplot(r_brick_ms,
          layout = c(4, 3),  # 4 columns x 3 rows
          names.attr = month.abb,  # Show month labels
          col.regions = my_colors,
          at = seq(global_min, global_max, length.out = 12),  # ensures same scale
          main = "Average Precipitation by Month (in)\n1991–2020",
          margin = FALSE,
          scales = list(
              x = list(draw = FALSE),  # fully disables x-axis labels and ticks
              y = list(draw = FALSE)   # fully disables y-axis labels and ticks
          )) 

crs(r_brick_ms) <- crs_backup

# turn into data frames and use ggplot ----
# Convert RasterBrick to SpatRaster (terra)
r_ms_terra <- rast(r_brick_ms)

# Convert to data frame (long format)
df_ms <- as.data.frame(r_ms_terra, xy = TRUE, na.rm = TRUE) |>
    pivot_longer(cols = -c(x, y),
                 names_to = "month",
                 values_to = "precip_in") |>
    mutate(month = factor(month, levels = month.abb))

my_colors2 <- colorRampPalette(brewer.pal(9, "GnBu"))(20)
# breaks <- seq(global_min, global_max, length.out = 12)



ggplot(df_ms, aes(x = x, y = y, fill = precip_in)) +
    geom_raster() +
    coord_fixed() +
    # scale_fill_distiller(palette = "GnBu", direction = 1, name = "Inches") +
    scale_fill_stepsn(colors = my_colors2, 
                      # breaks = round(breaks),
                      # limits = c(global_min, global_max),
                      name = "Inches") +
    facet_wrap(~month, ncol = 4) +
    labs(
        title = "Monthly Precipitation Normals",
        subtitle = "Averages across 1991–2020",
        x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
        axis.text = element_blank(),
        # strip.background = element_rect(fill = "white", color = "gray20", size = 0.5),
        panel.border = element_rect(color = "gray40",
                                    fill = NA, linewidth = 0.5),
        panel.grid = element_blank(),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        panel.spacing = unit(0.7, "lines")
    )

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
