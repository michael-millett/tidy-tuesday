
# Script Name:      Groundhog Day: TidyTuesday Week 5 (2024)
# Author:           Michael Millett
# Date Created:     2024-01-29
# Email:            mcmillett.extra@gmail.com


# OBJECTIVE:
#   - Create a plot showing year on the x-axis and total number of unique
#      predictions on the y-axis

# PURPOSE:
#   - To show that the popularity of groundhog day increased dramatically ~1997
#      as evidenced from the total number of prognositcators YoY


# Install pacman package ("package manager") if not installed -------------

if (!require("pacman")) install.packages("pacman")


# Load packages -----------------------------------------------------------

pacman::p_load(pacman,      # for loading packages
               tidyverse,   # for TidyVerse syntax and packages
               janitor,     # for cleaning column names efficiently
               showtext,    # for loading custom fonts from system
               ggimage,     # for inserting pictures on plot
               ggtext       # for extra text specification in plot
            )


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 5)
predictions <- tuesdata$predictions


# Data wrangling ----------------------------------------------------------

# Create new dataframe with total count of predictions for each year

df <- predictions %>%
  filter(complete.cases(shadow)) %>%
  group_by(year) %>%
  summarize(n_predictions = n_distinct(id))


# Remove extra data sources from environment

rm(tuesdata, predictions)


# Add fonts ---------------------------------------------------------------


font_add_google("Open sans", "open")
showtext_auto()

f2 <- "open"


# Create plot -------------------------------------------------------------


df %>%
  filter(year >= 1950) %>%

  ggplot() +

  geom_line(
    aes(x = year, y = n_predictions),
    linewidth = 0.75,
    stat = "smooth",
    color = "grey20",
    alpha = 0.3) +

  geom_jitter(
    aes(x = year, y = n_predictions),
    color = "#004f83",
    fill  = "#004f83",
    alpha = 0.5,
    size  = 2.5,
    width = 0.1,
    height = 0.1) +

  geom_segment(
    x     = 1993,
    xend  = 1993,
    y     = 0,
    yend  = 8,
    linewidth = 0.7,
    linetype = "dashed",
    color = "red") +

  geom_point(
    x = 1993,
    y = 9.25,
    shape = 23,
    size = 4,
    color = "black",
    fill = "gold") +

  geom_segment(
    x    = 2017,
    xend = 2024,
    y    = 2,
    yend = 2,
    size = 0.5,
    linewidth = 0.5,
    lineend  = "round",
    linejoin = "round",
    color = "grey75",
    arrow = arrow(
      angle = 25,
      length = unit(0.25, "cm"),
      type = "open")) +

  geom_segment(
    x         = 1951.5,
    xend      = 1951.5,
    y         = 70,
    yend      = 79,
    size      = 0.5,
    linewidth = 0.5,
    lineend   = "round",
    linejoin  = "round",
    color     = "grey75",
    arrow = arrow(
      angle   = 25,
      length  = unit(0.25, "cm"),
      type    = "open")) +

  annotate(
    geom   = "text",
    label  = "Year",
    size   = 3.5,
    x      = 2019.75,
    y      = 4,
    family = f2,
    color  = "grey30") +

  annotate(
    geom   = "text",
    label  = "Total\nPredictions",
    size   = 3.25,
    x      = 1953.5,
    y      = 75,
    family = f2,
    color  = "grey30",
    hjust  = 0) +

  annotate(
    geom   = "text",
    label  = "Groundhog feat. Bill Murray\nreleases in theatres (Feb 4th, 1993)",
    size   = 3.75,
    x      = 1970,
    y      = 33,
    family = f2,
    color  = "grey20") +

  geom_segment(
    x    = 1991,
    xend = 1993,
    y    = 25,
    yend = 10.5,
    linewidth = 0.1) +

  geom_segment(
    x    = 1985,
    xend = 1992.25,
    y    = 23,
    yend = 9.4,
    linewidth = 0.1) +

  scale_x_continuous(
    breaks = seq(1950, 2025, by = 25),
    limits = c(1950, 2025),
    expand = c(0.02, 0.2)) +

  scale_y_continuous(
    breaks = seq(0, 80, by = 20),
    limits = c(0, 80),
    expand = c(0, 0)) +

  xlab(NULL) +

  ylab(NULL) +

  labs(
    title    = "Number of Groundhog Day Prognosticator's Per Year",
    subtitle = "Total yearly predictions since 1950",
    caption  = "**Source:** Groundhog-Day.com | Graphic by Michael Millett") +

  coord_cartesian(clip = "off") +

  theme(
    plot.margin        = margin(c(35, 40, 0, 25)),
    panel.background   = element_rect(fill = "white", linewidth = 0),
    plot.background    = element_rect(fill = "white", linewidth = 0),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.30),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.30),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.ticks.y       = element_blank(),
    plot.title         = element_text(size = 21, family = f2, face = "bold", hjust = 0.5, vjust = 3),
    plot.subtitle      = element_text(size = 11, family = f2, color = "grey55", hjust = 0.5, vjust = 5),
    axis.text.x        = element_text(size = 11, family = f2, vjust = -1),
    axis.text.y        = element_text(size = 11, family = f2, hjust = 0.5),
    plot.caption       = element_textbox_simple(
      padding = margin(22, 0, 11, 0),
      margin = margin(0, 0, 0, 0),
      size = 10,
      family = f2,
      halign = 0.5,
      color = "grey20")) +

  geom_image(aes(image = "groundhog.png", x = 1988, y = 25.5), size = 0.3)

# Plot exported as PNG 876 x 676

p_unload(all)
graphics.off()
rm(list=ls())

?element_textbox_simple
