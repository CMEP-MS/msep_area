library(sf)
library(dplyr)

# Marcus Beck wrote most of this
# buffering was needed to get rid of the boundary between the two files

# in_path <- here::here("~/Desktop/MSEP only-20250402T141256Z-001/")
in_path <- here::here("Data",
                      "Watershed Boundaries",
                      "original")

# outlines only ----
msep_0317 <- st_read(here::here(in_path,
                                "0317_Outline.shp"), quiet = TRUE)
msep_0318 <- st_read(here::here(in_path,
                                "0318_Outline.shp"), quiet = TRUE)

# Transform to UTM
shapefile1_utm <- st_transform(msep_0317, 26916)
shapefile2_utm <- st_transform(msep_0318, 26916)

# Apply buffer approach with meter values
buffer_distance <- 0.5 # 50 cm buffer - Marcus wrote in 5 cm but there was still a speck
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
                                    "MSEP_outline.shp"))
