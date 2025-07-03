library(tidyverse)
library(tidycensus) #Need an api key from US Census Bureau.
library(sf)
library(dplyr)
library(tidyselect)
library(tigris)
options(tigris_use_cache = TRUE)
library(units)
library(mseptools)


var_2020pl <- load_variables(2020, "pl", cache = TRUE)
# P1_001N  total
# P1_004N  Black
# P1_006N  Asian
# P2_002N  Hispanic or Latino
var_2010pl <- load_variables(2010, "pl", cache = TRUE)
# P001001  is total population
# P001004  Black
# P001006  Asian
# P002002  Hispanic or Latino
var_2010sf1 <- load_variables(2010, "sf1", cache = TRUE)
# can get things like owner vs. renter, household size, etc. 
# but this dataset doesn't seem to be available for 2020 (yet)
var_2000pl <- load_variables(2000, "pl", cache = TRUE)
# PL001001

# Mississippi data ----

ests_2000 <- get_decennial(
    geography = "county",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "PL001001"),
    year = 2000,
    sumfile = "pl"
)

ests_2010 <- get_decennial(
    geography = "county",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "P001001"),
    year = 2010,
    sumfile = "pl"
)

ests_2020 <- get_decennial(
    geography = "county",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "P1_001N"),
    year = 2020,
    sumfile = "pl"
)

ests_2000b <- ests_2000 |> 
    separate_wider_delim(NAME, delim = " County, ",
                         names = c("County", "State")) |>  # no land area here, and need to split names
    st_drop_geometry() |>                    # keep the more recent geometry 
    select(GEOID,
           County,
           variable,
           estimate_2000 = value)

ests_2010b <- ests_2010 |> 
    mutate(Density = value / CENSUSAREA) |>  # per square mile
    st_drop_geometry() |>                    # keep the more recent geometry 
    select(GEOID,
           County = NAME.x,
           variable,
           estimate_2010 = value,
           density_2010 = Density)

ests_2020b <- ests_2020 
units(ests_2020b$ALAND) <- as_units("m2")

ests_2020c <- ests_2020b |> 
    mutate(area = as.numeric(set_units(ALAND, "mi2")),
           Density = value / area) |>  # per square mile
    select(GEOID,
           County = NAME.x,
           variable,
           estimate_2020 = value,
           density_2020 = Density)

ests_all <- full_join(ests_2000b, ests_2010b) |> 
    full_join(ests_2020c) |> 
    mutate(change_since2010_pct = (estimate_2020 - estimate_2010) / estimate_2010 * 100,
           change_since2000_pct = (estimate_2020 - estimate_2000) / estimate_2000 * 100) |> 
    st_as_sf()

# 2010-2020 ----
# how did things change in the state as a whole?
ests_all |> 
    filter(variable == "Total") |> 
    summarize(total_2010 = sum(estimate_2010),
              total_2020 = sum(estimate_2020),
              change_pct = (total_2020 - total_2010) / total_2010 * 100) |> 
    knitr::kable()


ests_all |> 
    filter(variable == "Total") |> 
    ggplot() +
    geom_sf(aes(fill = change_since2010_pct),
            alpha = 0.4) +
    geom_sf(data = msep_boundary,
            fill = NA,
            color = "navy",
            linewidth = 1) +
    theme_void() +
    khroma::scale_fill_sunset(reverse = TRUE,
                              midpoint = 0) +
    labs(title = "Population Change in MS by County, 2010 - 2020",
         caption = "Statewide population stayed virtually the same 
         at 2.96 million over the decade, decreasing by only 0.2%.
         Values based on the Decennial US Census 'pl' tables.",
         fill = "Change as % of \n2010 population")
ggsave(here::here("Maps", "population-change_2010-2020.png"))



# 2000-2020 ----
# how did things change in the state as a whole?
ests_all |> 
    filter(variable == "Total") |> 
    summarize(total_2000 = sum(estimate_2000),
              total_2020 = sum(estimate_2020),
              change_pct = (total_2020 - total_2000) / total_2000 * 100) |> 
    knitr::kable()


ests_all |> 
    filter(variable == "Total") |> 
    ggplot() +
    geom_sf(aes(fill = change_since2000_pct),
            alpha = 0.4) +
    geom_sf(data = msep_boundary,
            fill = NA,
            color = "navy",
            linewidth = 1) +
    theme_void() +
    khroma::scale_fill_sunset(reverse = TRUE,
                              midpoint = 0) +
    labs(title = "Population Change in MS by County, 2000 - 2020",
         caption = "Statewide population increased by 4% in this time period.
         Values based on the Decennial US Census 'pl' tables.",
         fill = "Change as % of \n2000 population")
ggsave(here::here("Maps", "population-change_2000-2020.png"))

ests_all |> 
    st_drop_geometry() |> 
    select(County,
           change_since2000_pct,
           change_since2010_pct,
           estimate_2000,
           estimate_2010,
           estimate_2020) |> 
    arrange(desc(change_since2000_pct)) |> 
    head(10) |> 
    knitr::kable()



ggplot(ests_all) +
    geom_sf(aes(fill = density_2020),
            alpha = 0.4) +
    geom_sf(data = msep_boundary,
            fill = NA,
            color = "navy",
            linewidth = 1) +
    theme_void() +
    khroma::scale_fill_sunset(reverse = TRUE,
                              midpoint = mean(ests_all$density_2020)) +
    labs(title = "Population Density: people per square mile, 2020",
         caption = "Values based on the Decennial US Census 'pl' tables.",
         fill = "Population Density")
ggsave(here::here("Maps", "population-density-2020.png"))

dens_and_change <- ests_all |> 
    st_drop_geometry() |> 
    select(County, density_2020, change_since2000_pct, estimate_2020) |> 
    arrange(desc(density_2020)) 
knitr::kable(dens_and_change)


# MSEP subset ----

msep_sf <- st_read(here::here("Data",
                              "Watershed Boundaries",
                              "combined",
                              "MSEP_outline.shp")) |> 
    dplyr::select(-all_of(ends_with("_1"))) |> 
    st_transform(crs = st_crs(ests_2020))

ms_tracts2020 <- get_decennial(
    geography = "tract",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "P1_001N"),
    year = 2020,
    sumfile = "pl"
)

ms_tracts_2000 <- get_decennial(
    geography = "tract",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "PL001001"),
    year = 2000,
    sumfile = "pl"
)

ms_tracts_2010 <- get_decennial(
    geography = "tract",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "P001001"),
    year = 2010,
    sumfile = "pl"
)


msep_tracts_2020 <- st_intersection(ms_tracts2020, msep_sf)
library(mapview)
mapview(msep_tracts_2020) +
    mapview(msep_sf,
            color = "orange",
            col.regions = "orange",
            alpha.regions = 0)

st_within(ms_tracts2020, msep_sf, sparse = FALSE)

msep_contained_tracts_2020 <- ms_tracts2020[st_within(ms_tracts2020, msep_sf, sparse = FALSE), ]
msep_inttracts <- ms_tracts2020[st_intersects(ms_tracts2020, msep_sf, sparse = FALSE), ]

mapview(msep_inttracts) +
mapview(msep_contained_tracts_2020,
        col.regions = "yellow") +
    mapview(msep_sf,
            color = "orange",
            col.regions = "orange",
            alpha.regions = 0,
            lwd = 2)

# general block groups?
blockgroups <- block_groups("MS")
cropped_blockgroups <- st_crop(blockgroups, st_bbox(msep_inttracts))
cropped_blockgroups <- st_make_valid(cropped_blockgroups)
msep_contained_blockgroups <- cropped_blockgroups[st_within(cropped_blockgroups, msep_sf, sparse = FALSE), ]
msep_int_blockgroups <- cropped_blockgroups[st_intersects(cropped_blockgroups, msep_sf, sparse = FALSE), ]

mapview(msep_int_blockgroups) +
    mapview(msep_contained_blockgroups,
            col.regions = "yellow") +
    mapview(msep_sf,
            color = "orange",
            col.regions = "orange",
            alpha.regions = 0,
            lwd = 2)

ests_2020_blockgroups <- get_decennial(
    geography = "block group",
    state = "MS",
    geometry = FALSE,
    keep_geo_vars = FALSE,
    variables = c("Total" = "P1_001N"),
    year = 2020,
    sumfile = "pl"
)

msep_int_ests <- filter(ests_2020_blockgroups,
                               GEOID %in% msep_int_blockgroups$GEOID)
msep_contained_ests <- filter(ests_2020_blockgroups,
                        GEOID %in% msep_contained_blockgroups$GEOID)

sum(msep_int_ests$value)
sum(msep_contained_ests$value)

# buffer the msep boundary ----
# get the block groups along the state line
msep_sf2 <- st_buffer(msep_sf, 500)  
msep2_contained_blockgroups <- cropped_blockgroups[st_within(cropped_blockgroups, msep_sf2, sparse = FALSE), ]

mapview(msep_int_blockgroups) +
    mapview(msep2_contained_blockgroups,
            col.regions = "yellow") +
    mapview(msep_sf,
            color = "orange",
            col.regions = "orange",
            alpha.regions = 0,
            lwd = 2)

msep_buffered_contained_ests <- filter(ests_2020_blockgroups,
                                       GEOID %in% msep2_contained_blockgroups$GEOID)
sum(msep_buffered_contained_ests$value)
