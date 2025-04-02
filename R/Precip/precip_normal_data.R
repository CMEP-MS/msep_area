# Make a map of 30-year normal annual precipitation
# based on data from PRISM group at Oregon State University

library(terra)
library(sf)
library(rnaturalearth)

prcp <- rast(here::here("Data",
                        "Precip",
                        "downloaded from PRISM annual normals",
                        "PRISM_ppt_30yr_normal_4kmM4_annual_bil.bil"))
# plot(prcp)
# print(prcp)

# crs(prcp)
# NAD83

# MSEP boundaries
msep <- read_sf(here::here("Data",
                           "Watershed Boundaries",
                           "combined",
                           "MSEP_outline.shp"))
msep <- st_transform(msep, crs(prcp))

# pull out only MS data from precip
ms_boundary <- ne_states(country = "United States of America", returnclass = "sf") %>%
    dplyr::filter(name == "Mississippi")

# Reproject the boundary to match the raster CRS
ms_boundary <- st_transform(ms_boundary, crs(prcp))


# crop and mask to mississippi
prcp_ms <- crop(prcp, ms_boundary)
prcp_ms <- mask(prcp_ms, ms_boundary)

prcp_ms_inches <- prcp_ms / 25.4


# Define a color palette
n_colors <- 9  # Number of color levels
# col_palette <- khroma::color("discrete rainbow", reverse = TRUE)(n_colors)
# col_palette <- khroma::color("sunset")(n_colors) # this would actually be great for temperature
# col_palette <- khroma::color("nightfall", reverse = TRUE)(n_colors)
# col_palette <- khroma::color("PRGn", reverse = TRUE)(n_colors) # this is one of my favorites
col_palette <- RColorBrewer::brewer.pal(n_colors, "YlGnBu")
# col_palette <- RColorBrewer::brewer.pal(n_colors, "GnBu")
# col_palette <- khroma::color("vik", reverse = TRUE)(n_colors)
# 

# Get raster value range
val_range <- range(values(prcp_ms_inches), na.rm = TRUE)

# use of image() ----
# Plot using image()
image(prcp_ms_inches, 
      col = col_palette, 
      main = "Average Annual Precipitation\n1991-2020",
      axes = FALSE,
      xlim = c(-91.5, -86.5))
box()
# add MSEP boundary
plot(st_geometry(msep), add = TRUE, col = NA, border = "gray20", lwd = 2)
# Add legends
legend("topright", 
       legend = round(seq(val_range[1], val_range[2], length.out = n_colors)),
       fill = col_palette[seq(1, n_colors, length.out = n_colors)], 
       title = "Inches")
legend("bottomright",
       legend = "MSEP \nBoundary", 
       col = "gray20", 
       lwd = 2, 
       bty = "n", 
       cex = 0.8)