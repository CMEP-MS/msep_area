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

msep_comb <- bind_rows(msep_0317_with_Subbasins, msep_0318_with_Subbasins)


st_write(msep_comb, here::here("Data",
                               "Watershed Boundaries",
                               "combined",
                               "MSEP_withSubbasins.shp"))
