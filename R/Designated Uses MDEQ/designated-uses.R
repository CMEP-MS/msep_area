library(tidyverse)
library(sf)
library(mapview)

linear <- st_read(here::here("Data", "Designated Uses MDEQ",
                             "SelectedWQSWaters",
                             "SelectedWQSLinearWaters.shp"))

polygons <- st_read(here::here("Data", "Designated Uses MDEQ",
                               "SelectedWQSWaters",
                               "SelectedWQSPolygonalWaters.shp"))

polygons |> 
    st_drop_geometry() |> 
    mutate(ATTR_VAL = case_when(is.na(ATTR_VAL) ~ "Wildlife",
                                .default = ATTR_VAL)) |> 
    summarize(.by = c(BASIN, ATTR_VAL),
              area = sum(SHAPE_AREA, na.rm = TRUE)) |> 
    mutate(acres = area/4047,
           sqmiles = area/(2.59*10^6))

polygons |> 
    st_drop_geometry() |> 
    summarize(.by = BASIN,
              area = sum(SHAPE_AREA, na.rm = TRUE)) |> 
    mutate(acres = area/4047,
           sqmiles = area/(2.59*10^6))

linear |> 
    st_drop_geometry() |> 
    mutate(ATTR_VAL = case_when(is.na(ATTR_VAL) ~ "Wildlife",
                                .default = ATTR_VAL)) |> 
    summarize(.by = c(BASIN, ATTR_VAL),
              area = sum(SHAPE_LEN, na.rm = TRUE))

mapview(linear,
        zcol = "ATTR_VAL")

mapview(polygons,
        zcol = "ATTR_VAL")
