library(sf)
library(dplyr)


in_path <- here::here("Data",
                      "Watershed Boundaries",
                      "original")

# with subbasins ----
msep_0317_with_Subbasins <- st_read(here::here(in_path,
                                               "0317_with_Subbasins.shp"), quiet = TRUE)
msep_0318_with_Subbasins <- st_read(here::here(in_path,
                                               "0318_with_Subbasins.shp"), quiet = TRUE)


# Transform to UTM
shapefile1_utm <- st_transform(msep_0317_with_Subbasins, 26916)
shapefile2_utm <- st_transform(msep_0318_with_Subbasins, 26916)

# Apply buffer approach with meter values
buffer_distance <- 0.05 
shapefile1_buffered <- st_buffer(shapefile1_utm, buffer_distance)
shapefile2_buffered <- st_buffer(shapefile2_utm, buffer_distance)

# Union the buffered geometries
combined_utm <- st_union(shapefile1_buffered, shapefile2_buffered)
plot(st_geometry(combined_utm))

# Clean up the result with negative buffer
combined_clean_utm <- st_buffer(combined_utm, -buffer_distance)

# Transform back to original CRS if needed
combined_clean <- st_transform(combined_clean_utm, 4269)
plot(st_geometry(combined_clean))

st_write(combined_clean, here::here("Data",
                                    "Watershed Boundaries",
                                    "combined",
                                    "MSEP_withSubbasins.shp"))
