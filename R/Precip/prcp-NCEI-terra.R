library(terra)
library(rnaturalearth)
library(sf)
library(RColorBrewer)
library(rasterVis)
library(tmap)

# precip data ----
dat_nc3 <- rast(here::here("Data",
                             "Precip",
                             "downloaded from NCEI monthly normals",
                             "prcp-1991_2020-monthly-normals-v1.0.nc"))

dat_annual <- dat_nc3[["annprcp_norm"]]
dat_monthly <- dat_nc3[[grep("mlyprcp_norm", names(dat_nc3))]]
names(dat_monthly) <- month.abb

plot(dat_annual)
plot(dat_monthly)  # notice scales are different in every facet

image(dat_annual)
image(dat_monthly)  # only does 1 - would have to loop through

# convert to inches ----
annual_in <- dat_annual / 25.4
monthly_in <- dat_monthly / 25.4


# crop to MS ----

# ms from rnaturalearth
ms_rne <- ne_states(country = "United States of America", returnclass = "sf") |> 
    dplyr::filter(name == "Mississippi")
ext(ms_rne)
crs(ms_rne)  # WGS 84

# Reproject the boundary to match the raster CRS (even though it was already the same, just being safe)
ms_rne <- sf::st_transform(ms_rne, crs(dat_annual))
ext(ms_rne)

annual_ms_in <- crop(annual_in, ms_rne)
annual_ms_in <- mask(annual_ms_in, ms_rne)
plot(annual_ms_in)


monthly_ms_in <- crop(monthly_in, ms_rne)
monthly_ms_in <- mask(monthly_ms_in, ms_rne)
plot(monthly_ms_in)


# uniform color scales etc. ----

myPal <- RColorBrewer::brewer.pal('GnBu', n=9)
myTheme <- rasterTheme(region = myPal)
levelplot(monthly_ms_in, par.settings = myTheme)

# monthly ----
levelplot(monthly_ms_in,
          par.settings = myTheme,
          layout = c(4, 3),  # 4 columns x 3 rows
          main = "Average Precipitation by Month (in)\n1991–2020",
          colorkey = list(title = list("inches",
                                       fontsize = 8),
                          space = "bottom"),
          margin = FALSE,
          xlab = NULL,
          ylab = NULL,
          scales = list(
              x = list(draw = FALSE),  
              y = list(draw = FALSE)   
          )
) 


# annual ----
levelplot(annual_ms_in,
          par.settings = myTheme,
          main = "Average Annual Precipitation (in)\n1991–2020",
          colorkey = list(title = list("inches",
                                       fontsize = 8),
                          space = "bottom"),
          margin = FALSE,
          xlab = NULL,
          ylab = NULL,
          scales = list(
              x = list(draw = FALSE), 
              y = list(draw = FALSE)  
          )
) 

# with msep boundary ----
msep <- read_sf(here::here("Data",
                           "Watershed Boundaries",
                           "combined",
                           "MSEP_outline.shp"))
msep <- st_transform(msep, crs(annual_ms_in))
msep_sp <- as_Spatial(st_geometry(msep))


levelplot(annual_ms_in,
          par.settings = myTheme,
          main = "Average Annual Precipitation (in)\n1991–2020",
          colorkey = list(title = list("inches",
                                       fontsize = 8),
                          space = "right"),
          margin = FALSE,
          xlab = NULL,
          ylab = NULL,
          scales = list(
              x = list(draw = FALSE), 
              y = list(draw = FALSE)  
          )
) +
    latticeExtra::layer(sp::sp.polygons(msep_sp))

# tmap ----
# Set tmap mode to "plot" for static maps (vs. "view" for interactive)
tmap_mode("plot")

# Plot
tm_shape(monthly_ms_in) +
    tm_raster(col.scale = tm_scale_continuous(values = "brewer.gn_bu"),
              col.free = FALSE,  # make the colors the same in every facet
              col.legend = tm_legend(title = "Inches",
                                     position = tm_pos_out("right"))) +
    tm_shape(msep) +
    tm_borders(col = "black", lwd = 2) +
    tm_facets(ncol = 4, nrow = 3) +
    tm_title("Average Precipitation by Month\n1991–2020")
