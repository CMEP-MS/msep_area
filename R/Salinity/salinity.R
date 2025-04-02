# Make maps of salinity data in the Mississippi Sound
# based on data from Gulf Atlas - Nelson 2015


library(sf)
library(dplyr)

dat <- st_read(here::here("Data",
                          "Salinity",
                          "downloaded from Gulf Atlas",
                          "MS_SalSeasons.shp"))

# Ensure SAL_LOW and SAL_HIGH are factors with the correct levels
dat$SAL_LOW <- factor(dat$SAL_LOW, levels = c("0-0.5", "0.5-5", "05-15", "15-25", ">25"))
dat$SAL_HIGH <- factor(dat$SAL_HIGH, levels = c("0-0.5", "0.5-5", "05-15", "15-25", ">25"))

colors <- RColorBrewer::brewer.pal(5, "GnBu")
names(colors) <- c("0-0.5", "0.5-5", "05-15", "15-25", ">25")


# plot on top of each other
par(mfrow = c(2, 1)) 

plot(dat["SAL_LOW"], col = colors[dat$SAL_LOW], main = "Low Salinity Period", reset = FALSE)
plot(dat["SAL_HIGH"], col = colors[dat$SAL_HIGH], main = "High Salinity Period")

# Add the shared color legend 
par(xpd = TRUE)
legend("bottomright", horiz = TRUE,
       legend = names(colors), fill = colors, title = "Salinity (ppt)")

# Reset to default plotting layout
par(mfrow = c(1, 1)) # Reset to default layout
