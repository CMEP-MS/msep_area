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

ests_2010 <- get_decennial(
    geography = "county",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "P001001",
                  "Black" = "P001004",
                  "Asian" = "P001006",
                  "Hispanic" = "P002002"),
    year = 2010,
    sumfile = "pl",
    output = "wide"
)

ests_2020 <- get_decennial(
    geography = "county",
    state = "MS",
    geometry = TRUE,
    keep_geo_vars = TRUE,
    variables = c("Total" = "P1_001N",
                  "Black" = "P1_004N",
                  "Asian" = "P1_006N",
                  "Hispanic" = "P2_002N"),
    year = 2020,
    sumfile = "pl",
    output = "wide"
)

ests_2010b <- ests_2010 |> 
    mutate(Density = Total / CENSUSAREA) |>  # per square mile
    st_drop_geometry() |>                    # keep the more recent geometry 
    pivot_longer(c(Total:Density),
                 names_to = "variable",
                 values_to = "estimate_2010") |> 
    select(GEOID,
           County = NAME.x,
           variable,
           estimate_2010)

ests_2020b <- ests_2020 
units(ests_2020b$ALAND) <- as_units("m2")

ests_2020c <- ests_2020b |> 
    mutate(area = as.numeric(set_units(ALAND, "mi2")),
           Density = Total / area) |>  # per square mile
    relocate(c(area, geometry), .after = Density) |> 
    pivot_longer(c(Total:Density),
                 names_to = "variable",
                 values_to = "estimate_2020") |> 
    select(GEOID,
           County = NAME.x,
           variable,
           estimate_2020)

ests_all <- full_join(ests_2010b, ests_2020c) |> 
    mutate(population_change = estimate_2020 - estimate_2010,
           population_change_pct = population_change / estimate_2010 * 100) |> 
    st_as_sf()

ests_all |> 
    filter(variable == "Total") |> 
    ggplot() +
    geom_sf(aes(fill = population_change_pct),
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
ggsave(here::here("Maps", "population-change.png"))

# how did things change in the state as a whole?
ests_all |> 
    filter(variable == "Total") |> 
    summarize(total_2010 = sum(estimate_2010),
              total_2020 = sum(estimate_2020),
              change_pct = (total_2020 - total_2010) / total_2010 * 100) |> 
    knitr::kable()

