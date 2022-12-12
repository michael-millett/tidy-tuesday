
# Script Name:      heatmap_nyc_tidy_tuesday
# Purpose:          Geometrically represent passenger elevators in NYC
# Author:           Michael Millett
# Date Created:     2022-12-12

# License: MIT License 2022 Michael Millett
# Email: mcmillett.extra@gmail.com

# Load packages needed for analysis
pacman::p_load(pacman, tidyverse, ggtext, broom, rio,
               ggmap, osmdata, geojsonio, sf, viridis)

# Load the spatial data for NYC map
spdf_file <- geojson_read(
  "nyc_zipcodes_polygon.geojson",
  what = "sp")

# Create a new dataframe with the NY geojuson data
stats_df <- as_tibble(spdf_file)

# Convert the spatial data to a tibble by using broom::tidy
spdf_file <- tidy(
  spdf_file,
  region = "ZIPCODE")

# Import the elevator data
#   - Remove the outlier observation for elevator in maryland
#   - Mutate zip codes so that they match spdf_file (nyc shape data)
#   - filter for active passenger elevators
#   - select relevent columns; zip_code, latitude, longitude
#   - group_by zip_code so we can get totals
#   - count
df_elevators <- import("data/2022_49_elevators.rds") %>%
  filter(longitude >= -74.5) %>%
  mutate(zip_code = str_sub(zip_code, 1, 5)) %>%
  filter(device_type == "passenger elevator (p)",
         device_status == "a") %>%
  select(zip_code, latitude, longitude) %>%
  group_by(zip_code) %>%
  count(zip_code) %>%
  ungroup() %>%
  rename(total_elevators = n)

# Rename zip_code in stats_df to prep for left_join
stats_df <- stats_df %>%
  select(ZIPCODE) %>%
  rename(zip_code = ZIPCODE)

# Create new dataframe with left_join of stats_df
#   - then sum up totals for each zip_code elevators are in
nyc_passenger_elevators <- left_join(stats_df,
                                     df_elevators,
                                     by = "zip_code") %>%
  group_by(zip_code) %>%
  summarize(
    total_elevators = sum(total_elevators)) %>%
  arrange(desc(total_elevators)) %>%
  ungroup()

# Now we're ready to plot!

####
## Create a map object and plot the nyc geometry data with elevator fill
####

plot_nyc <- ggplot() +
  geom_polygon(data = spdf_file %>%
                 left_join(nyc_passenger_elevators, by = c("id"="zip_code")),
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = total_elevators),
               color = "white",
               size = 0.2) +
  theme_void() +
  coord_map() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Passenger Elevators in New York City",
       subtitle = "Regions Separated by Zip Code<br>",
       fill = "**Number of<br>Elevators**",
       caption = "Data: Elevators Package by Emil Hvitfeldt & NYC Open Data | Graphic by Michael Millett") +
  theme(plot.title = element_markdown(face = "bold", size = 16),
        plot.subtitle = element_markdown(color = "grey50", size = 10, lineheight = 0.5),
        legend.title = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(color = "grey50", size = 8, hjust = 1),
        plot.margin = margin(t = 20, r = 30, b = 20, l = 30, unit = "pt"),
        panel.background = element_rect(fill = "white", colour = "grey50", size = 0.8))

plot_nyc

p_unload(all)
rm(list=ls())
graphics.off()
