
# Script Name:      Rainfall Heatmap of US using IDW Spatial Interpolation
# Purpose:          TidyTuesday Challenege Week 51 - Weather Forecasts in the US
# Author:           Michael Millett
# Date Created:     2022-12-25 (Merry Christmas!) :-)
# Email:            mcmillett.extra@gmail.com
# References:       Manuel Gimond, Intro to GIS and Spatial Analysis (2022)
#                   (code adapted from section J of his book for interpolation)
#                   https://mgimond.github.io/Spatial/interpolation-in-r.html

# OBJECTIVE:
#   - Predict rainfall across the US regions using Krig spatial interpolation
#     of non-sampled cities.

# NOTES:
#   - I used custom fonts downloaded from Google Fonts for the graphic
#   - You'll need to download and then import them using the showtext package
#   - These are the fonts used:
#       - Title:          Assistant Bold
#       - Subtitle:       Assistant
#       - Legend Title:   Source Sans Pro Bold
#       - Legend Text:    Chivo Mono
#       - Caption:        DM Mono

# Load packages needed for analysis
pacman::p_load(pacman,      # for loading packages
               tidyverse,   # for TidyVerse syntax and packages
               magrittr,    # for additional pipe operators
               rio,         # for importing/exporting data
               sf,          # for handling mapdata & shapefiles
               showtext,    # for loading custom fonts from system
               sp,          # for additional map data functions
               gstat)       # for geostatistic functions

showtext_auto()

# Load map projection (downloaded from US Census Bureau)
states_map <- st_read("us_states_shapefiles_20m/cb_2021_us_state_20m.shp")

# Filter out non-continous states and territories on US map
states_map %<>%
  filter(!(STUSPS %in% c("AK", "HI", "VI", "GU", "AS", "MP", "PR")))

# Load in dataset from cities
cities <- import("data/cities.rds") %>%
  filter(!(state %in% c("AK", "HI", "VI", "GU", "AS", "MP", "PR")))

# Create a new dataframe with lon/lat of each city along with their average
# rainfall amounts
df_rain <- cities %>%
  dplyr::select(x = lon,
                y = lat,
                z = avg_annual_precip)

# Convert the map shapefile to a SpatialPolygonsDataFrame
states_map <- as(states_map, "Spatial")

# Convert the df_rain dataframe to a SpatialPointsDataFrame
coordinates(df_rain) <- ~ x + y

# Create an empty grid where n is the total number of cells
#   - use a lower n value for less system resources
#   - when n = 1,000,000 it took my m1 MacBook ~ 10 seconds to create grd
grd              <- as.data.frame(spsample(states_map, "regular", n=1000000))
names(grd)       <- c("x", "y") # rename the columns to x and y in grd
coordinates(grd) <- c("x", "y") # convert grd dataframe to SpatialPoints
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(df_rain) <- proj4string(df_rain) # Temp fix until new proj env is adopted
proj4string(grd)     <- proj4string(df_rain)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(z ~ 1, df_rain, newdata = grd, idp = 2.0)

# Convert to raster object then clip to the US Map
r       <- raster::raster(P.idw)
r.m     <- terra::mask(r, states_map)

# Create a new dataframe that is a raster points of the rain predictions
map.p <- raster::rasterToPoints(r.m)

df <- data.frame(map.p)

colnames(df) <- c("lon", "lat", "pred_rain") # set column names to new df

# Round pred_rain values to lowest interval of 5 and set any values greater than
# 70 down to 65 as the max (to accomodate color bins)
df <- df %>%
  mutate(
    bin = plyr::round_any(pred_rain, 5, f = floor)) %>%
  mutate(bin = if_else(bin == 70, 65, bin))

custom_colors <- c("#9d1742", "#ca304c", "#e55549",
                            "#f6804c", "#fdb365", "#fcd681",
                            "#fef3a9", "#f5fbb0", "#daf199",
                            "#b0dea2", "#7acaa3", "#4da5b1",
                            "#3a7cb7", "#5f4ea2")

# Custom breaks for the legend text
legend_breaks <- c("Less than 5 in",
                   " 5 - 10 in", "10 - 15 in", "15 - 20 in",
                   "20 - 25 in", "25 - 30 in", "30 - 35 in",
                   "35 - 40 in", "40 - 45 in", "45 - 50 in",
                   "50 - 55 in", "55 - 60 in", "60 - 65 in",
                   "More than 65 in")

# Creating the plot
plot <- ggplot() +
  geom_tile(data = df, aes(lon, lat, fill = factor(bin))) +
  geom_point(data = cities,
             aes(x = lon, y = lat),
             size = 0.2,
             alpha = 0.5) +
  scale_fill_manual(values = custom_colors, labels = legend_breaks, ) +
  coord_quickmap() +
  labs(
    title = "Average Rainfall Across the United States",
    subtitle = "Non-sampled Regions Predicted Using IDW Interpolation (Samples used from 2021)",
    caption = "Source: US National Weather Service & US Census Bureau | Graphic by Michael Millett",
    fill = "Total Rainfall"
  ) +
  theme_void() +
  theme(legend.key = element_rect(fill = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.94, 0.35))

# Creating a Custom theme to specify graph aesthetics
custom_theme <-
  theme(
    plot.margin = margin(c(0, 20, 10, 20)),
    plot.title = element_text(family = "assistant", face = "bold", size = 30, vjust = -1),
    plot.subtitle = element_text(family = "assistant", size = 12, vjust = -3, color = "grey40"),
    legend.title = element_text(family = "source_sans_pro", face = "bold", size = 11, hjust = 0.1, vjust = 0.5),
    legend.text = element_text(family = "chivo_mono", size = 9),
    plot.caption = element_text(family = "dm_mono", size = 12)
  )

# Rendering the plot and the theme
plot + custom_theme

# Plot exported to PDF with dimensions 12x7.3 in (to maintain font scaling)

# CLEAN UP
# p_unload(all)
# graphics.off()
# rm(list=ls())
