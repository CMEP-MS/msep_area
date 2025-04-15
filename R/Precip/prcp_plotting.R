library(ncdf4)
library(raster)
library(rasterVis)
library(gridExtra)

prcp1 <- raster(here::here("NCEI Gridded Precip Data",
                           "prcp-1901_2000-msep.nc"))
prcp2 <- raster(here::here("NCEI Gridded Precip Data",
                           "prcp-1991_2020-msep.nc"))
prcp3 <- raster(here::here("NCEI Gridded Precip Data",
                           "prcp-2006_2020-msep.nc"))

# Find the global min and max values across all rasters
global_min <- min(cellStats(prcp1, "min"), cellStats(prcp2, "min"), cellStats(prcp3, "min"))
global_max <- max(cellStats(prcp1, "max"), cellStats(prcp2, "max"), cellStats(prcp3, "max"))


par(mfrow = c(1, 3))

image(prcp1, main = "1901-2000 Normal Monthly Precip", zlim = c(global_min, global_max))
image(prcp2, main = "1991-2020 Normal Monthly Precip", zlim = c(global_min, global_max))
image(prcp3, main = "2006-2020 Normal Monthly Precip", zlim = c(global_min, global_max))


# different way to plot ----
breaks <- seq(global_min, global_max, length.out = 100)
plot1 <- levelplot(prcp1, main = "1901-2000 Normal Monthly Precip", at = breaks)
plot2 <- levelplot(prcp2, main = "1991-2020 Normal Monthly Precip", at = breaks)
plot3 <- levelplot(prcp3, main = "2006-2020 Normal Monthly Precip", at = breaks)

windows()
grid.arrange(plot1, plot2, plot3, ncol = 3)
