
# Script Name:      FeederWatch Common Birds Spotted in KY (11/20 - 4/21)
# Purpose:          TidyTuesday Challenge Week 2 - FeederWatch
# Author:           Michael Millett
# Date Created:     2023-Jan-16
# Email:            mcmillett.extra@gmail.com

# OBJECTIVE:
#   - Calculate the total number birds spotted in KY by FeederWatch members
#     and visualize them using a bar chart of the top 10 most commonly sighted
#     species (only counting validated submissions)

# Load packages
pacman::p_load(pacman,      # for loading packages
               tidyverse,   # for TidyVerse syntax and packages
               magrittr,    # for additional pipe operators
               rio,         # for importing/exporting data
               showtext,    # for loading custom fonts from system
               ggimage,     # for inserting pictures on plot
               janitor,     # for cleaning column names efficiently
               ggtext       # for extra text specification in plot
)


# LOADING CUSTOM FONTS ----------------------------------------------------

# These are the fonts used (NOTE: only Roboto is open-source)
#   - Title:      Myriad-Pro      (Adobe fonts)
#   - Subtitle:   Roboto          (Google fonts)
#   - Axis Text:  Aglet Mono      (Adobe fonts)
#   - Caption:    Aglet Mono      (Adobe fonts)

font_add(
  "aglet_mono",
  regular = "../../../system_files/ggplot_fonts/Aglet-Mono-Features.otf")

font_add(
  "myriad_pro",
  regular = "../../../system_files/ggplot_fonts/MyriadPro-Bold.otf")

font_add(
  "roboto",
  regular = "../../../system_files/ggplot_fonts/Roboto-Regular.ttf")

showtext_auto() # function to enable the loaded fonts

# Storing fonts as objects for easy reference when making plot
f1 <- "myriad_pro"
f2 <- "roboto"
f3 <- "aglet_mono"

# IMPORT DATA -------------------------------------------------------------

# Import bird sightings (feeder) dataset
df <- import("data/feeder.rds") %>%
  clean_names() %>%
  filter(subnational1_code == "US-KY",
         valid == 1) %>%
  select(species_code, how_many)

# Import species codebook to join their names with species code in df
# (downloaded from FeederWatch dictionary codebook)
bird_names <- read_csv("data/species_codes.csv", skip = 1) %>%
  clean_names() %>%
  select(species_code, primary_com_name)

# Do a left_join to add bird names to sightings df
df %<>%
  left_join(., bird_names, by = "species_code")

# Remove the bird_names dataset
rm(bird_names)

# Calculate the total number of sightings for each bird species
df %<>%
  rename(bird_name = primary_com_name) %>%
  group_by(bird_name) %>%
  summarize(
    total_spotted = sum(how_many)
  ) %>%
  arrange(desc(total_spotted)
          ) %>%
  mutate(bird_name = as_factor(bird_name),
         bird_name = fct_reorder(bird_name, total_spotted))

# Making custom labels for y-axis
y_labels <- c("0", "50", "100", "150", "200", "250", "300")


# BAR PLOT ----------------------------------------------------------------

plot <- ggplot(data = head(df, 15),
               aes(x = bird_name,
                   y = total_spotted)) +
  geom_col(fill = "#0f4c81",
           alpha = 0.8) +
  scale_y_continuous(
    limits = c(0, 400),
    breaks = seq(0, 300, 50),
    minor_breaks = NULL,
    labels = y_labels,
    position = "right"
  ) +
  coord_flip(clip = "off") +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Most Common Birds Sighted in KY",
       subtitle = "Sightings Reported from Project FeederWatch Members (Nov 2020 - Apr 2021)",
       caption = "Source: Project FeederWatch | Graphic by Michael Millett")

plot <- plot + theme(
  plot.margin = margin(c(20, 40, 30, 20)),
  panel.background = element_rect(fill = "grey90", linewidth = 0),
  plot.background = element_rect(fill = "grey90", linewidth = 0),
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_line(color = "grey78", linewidth = 0.5),
  axis.ticks.x = element_line(color = "grey78", size = 0.5),
  axis.ticks.y = element_blank(),
  plot.title = element_text(size = 32, family = f1, hjust = -1.4, margin = margin(2, 2, 2, 2)),
  plot.subtitle = element_text(size = 13, family = f2, color = "grey40", hjust = -0.45, margin = margin(0, 2, 20, 2)),
  axis.text.x = element_text(size = 13, family = f3, ),
  axis.text.y = element_text(size = 11, family = f3, margin = margin(0, -15, 0, 0)),
  plot.caption = element_text(size = 10, family = f3, vjust = -5, hjust = 0.2, color = "grey20")
)

plot <- plot +
  geom_image(aes(x = 14.75, y = 375, image = "pictures/goldfinch.png"), size = 0.18) +
  geom_image(aes(x = 11.5, y = 375, image = "pictures/dove.png"), size = 0.18) +
  geom_image(aes(x = 8.25, y = 375, image = "pictures/finch.png"), size = 0.18) +
  geom_image(aes(x = 5, y = 375, image = "pictures/cardinal.png"), size = 0.18) +
  geom_image(aes(x = 1.75, y = 375, image = "pictures/sparrow.png"), size = 0.18) +
  annotate("label",
           x = 13.35,
           y = 375,
           size = 4,
           label = "American Goldfinch",
           family = f3,
           color = "grey20",
           fill = "grey90") +
  annotate("label",
           x = 10.1,
           y = 375,
           size = 4,
           label = "Mourning Dove",
           family = f3,
           color = "grey20",
           fill = "grey90") +
  annotate("label",
           x = 6.85,
           y = 375,
           size = 4,
           label = "House Finch",
           family = f3,
           color = "grey20",
           fill = "grey90") +
  annotate("label",
           x = 3.6,
           y = 375,
           size = 4,
           label = "Northern Cardinal",
           family = f3,
           color = "grey20",
           fill = "grey90") +
  annotate("label",
           x = 0.35,
           y = 375,
           size = 4,
           label = "House Sparrow",
           family = f3,
           color = "grey20",
           fill = "grey90")

plot

# Exported as pdf w/ dimensions 9.83 x 8.86

# p_unload(all)
# rm(list=ls())
# graphics.off()
