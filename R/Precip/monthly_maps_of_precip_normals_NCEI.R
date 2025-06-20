library(terra)
library(rnaturalearth)
library(sf)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyterra)


# precip data ----
dat_nc <- rast(here::here("Data",
                          "Precip",
                          "downloaded from NCEI monthly normals",
                          "prcp-1991_2020-monthly-normals-v1.0.nc"))

dat_monthly <- dat_nc[[grep("mlyprcp_norm", names(dat_nc))]]
names(dat_monthly) <- month.abb

# convert to inches
monthly_in <- dat_monthly / 25.4

# crop to MS ----
# ms from rnaturalearth
ms_rne <- ne_states(country = "United States of America", returnclass = "sf") |> 
    dplyr::filter(name == "Mississippi")
ms_rne <- sf::st_transform(ms_rne, crs(dat_monthly))

monthly_ms_in <- crop(monthly_in, ms_rne)
monthly_ms_in <- mask(monthly_ms_in, ms_rne)


# msep outline ----
msep <- read_sf(here::here("Data",
                           "Watershed boundaries",
                           "combined",
                           "MSEP_outline.shp"))
msep <- st_transform(msep, crs(monthly_ms_in))

p <- ggplot() +
    geom_spatraster(data = monthly_ms_in) +
    facet_wrap(~lyr) +
    scale_fill_distiller(palette = "GnBu", direction = 1,
                         na.value = NA) +
    theme_void() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_rect(fill = NA,
                                          color = NA),
          strip.text = element_text(face = "bold")) +
    labs(title = "Monthly Precipitation Normals",
         subtitle = "1991-2020 average",
         fill = "Inches")

p

ggsave(plot = p, file = here::here("Faceted precip.png"),
       height = 11, width = 8,
       units = "in",
       dpi = 800)
# 11 high x 8 wide



# loop through each month and make the plot

my_colors <- colorRampPalette(brewer.pal(9, "GnBu"))(11)  # Smooth gradient
global_min <- min(values(monthly_ms_in), na.rm = TRUE)
global_max <- max(values(monthly_ms_in), na.rm = TRUE)


ggplot() +
    geom_spatraster(data = monthly_ms_in) +
    facet_wrap(~lyr) +
    scale_fill_distiller(palette = "GnBu", direction = 1,
                         limits = c(global_min, global_max),
                         na.value = NA) +
    geom_sf(data = ms_rne,
            fill = NA,
            col = "black",
            linewidth = 0.7) +
    theme_void() +
    labs(fill = "Inches")
ggsave(filename = here::here("Maps", "Precip_monthly",
                             "00_all_months.png"),
       height = 8,
       width = 8,
       units = "in",
       dpi = 800)

for(i in seq_along(names(monthly_ms_in))){
    # pull the layer
    tmp <- monthly_ms_in[[i]]
    month_nm <- names(monthly_ms_in)[i]
    
    p <- ggplot() +
        geom_spatraster(data = tmp) +
        scale_fill_distiller(palette = "GnBu", direction = 1,
                             limits = c(global_min, global_max),
                             na.value = NA) +
        geom_sf(data = ms_rne,
                fill = NA,
                col = "black",
                linewidth = 0.7) +
        theme_void() +
        theme(legend.position = "none")
    
    flnm <- paste0(sprintf("%02d", i), "_", month_nm, ".png") 
    
    ggsave(filename = here::here("Maps",
                                 "Precip_monthly",
                                 flnm),
           plot = p,
           height = 11,
           width = 8,
           units = "in",
           dpi = 800)
    
    
}
