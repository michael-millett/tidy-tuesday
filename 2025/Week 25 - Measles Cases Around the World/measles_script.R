
# Script Name:      TidyTuesday: Measles Cases (Week 25 / 2025)
# Author:           Michael Millett
# Date Created:     6/24/2025

# OBJECTIVE:
#   - Create a line graph that shows the general trend of measles cases per
#     year to compare the number of provisional cases reported vs the annual
#     cases reported to the World Health Organization


# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(signal)
library(gridExtra)
library(grid)
library(showtext)
library(ggtext)

pacman::p_load(gridExtra, grid, showtext, ggtext)

# LOAD DATA ---------------------------------------------------------------


if (!require("tidytuesdayR")) install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load(2025, week = 25)

df <- tuesdata$cases_year %>% 
  group_by(year) %>% 
  summarize(
    cases = sum(measles_lab_confirmed, measles_epi_linked)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(year != 2025) %>% 
  mutate(type = rep("Provisionally Reported", 13))

rm(tuesdata)

# Pull Measles Annual Reported Cases (as of 7/8/2025)

# https://immunizationdata.who.int/global/wiise-detail-page/measles-reported-cases-and-incidence?CODE=Global&YEAR=

reported <- tibble(year = c(2012, 2013, 2014, 2015, 2016, 2017, 
                            2018, 2019, 2020, 2021, 2022, 2023, 2024),
                   cases = c(212376,275307, 282078, 214808, 132490, 173457,
                             359295, 873373, 159240, 123152, 206775, 669083, 675533),
                   type = rep("Annually Reported", 13))


# PCHIP INTERPOLATION -----------------------------------------------------

# Interpolation for provisional cases

seq_years <- seq(from = 2012,          # Starting X Value in data frame
                 to   = 2024,          # Max X Value in data frame
                 length.out = 1000)    # Vector length with interpolated values from 2012 to 2025

y_interpolated <- signal::pchip(x  = df$year,
                                y  = df$cases,
                                xi = seq_years)

provisional_interpolated <- tibble(x = seq_years, 
                          y = y_interpolated)

rm(seq_years, y_interpolated)

# Interpolation for annual reported cases

report_years <- seq(from = 2012,
                    to   = 2024,
                    length.out = 1000)

report_interpolated = signal::pchip(x = reported$year,
                                    y = reported$cases,
                                    xi = report_years)

annual_interpolated <- tibble(x = report_years, 
                           y = report_interpolated)

rm(report_years, report_interpolated)


# LOAD FONTS --------------------------------------------------------------

font_add("open sans bold", "../../../../System/fonts/open_sans/static/OpenSans/OpenSans-Bold.ttf")

font_add("open sans regular", "../../../../System/fonts/open_sans/static/OpenSans/OpenSans-Regular.ttf")

showtext_auto()

f1 <- "open sans bold"
f2 <- "open sans regular"


# BIND DATA ---------------------------------------------------------------

df <- df %>% 
  rbind(reported)

# CREATE PLOT -------------------------------------------------------------

colors <- c("dodgerblue", "#E15759")


plot <- ggplot() +
  
  geom_point(data = df,
             aes(x = year, y = cases, fill = type),
             color = "white",
             size = 4,
             shape = 21) +
  
  geom_line(data = provisional_interpolated,
            aes(x = x, y = y),
            color = "#E15759",
            linewidth = 1) +
  
  geom_line(data = annual_interpolated,
            aes(x = x, y = y),
            color = "dodgerblue",
            linewidth = 1) +
  
  scale_x_continuous(
    breaks = seq(from = 2012, to = 2025, by = 1),
    expand = c(0.025, 0.025)) +
  
  scale_y_continuous(
    labels = c("0", "", "200K", "", "400K", "", "600K", "", "800K", "", "1 million"),
    breaks = seq(from = 0, to = 1000000, by = 100000),
    limits = c(0, 1025000),
    expand = c(0, 0)) +
  
  labs(
    title = "Global Measles Cases by Year",
    subtitle = "Annual reported total of measles cases worldwide (2012–2024)",
    y = NULL,
    x = NULL,
    caption = "Provisional case counts based on monthly WHO reporting as of June 2025") +
    
  scale_fill_manual(values = colors) +
  
  theme(
    
    plot.margin = margin(15, 30, 10, 15),
    
    plot.background = element_rect(color = "grey88"),
    
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "grey88"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor.y = element_blank(),
    
    plot.title = element_textbox(family = f1,
                                 size = 48,
                                 hjust = 0.5),
    
    
    plot.title.position = "plot",
    
    
    plot.subtitle = element_textbox(family = f2,
                                    size = 20,
                                    hjust = 0.5,
                                    margin = margin(b = 20),
                                    lineheight = 1.3),
    
    plot.caption = element_textbox(family = f2,
                                   size = 14,
                                   margin = margin(t = 20, b = 5),
                                   color = "#B8B8B8"),
    
    legend.position = c(0.27, 0.78),
    legend.key = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(family = f2, size = 14),
    legend.key.size = unit(1.75, "lines"),
    legend.text.align = 0,
    axis.title = element_blank(),
    axis.text = element_text(family = f2, size = 18),
    axis.text.y = element_text(hjust = 1, margin = margin(r = 15)),
    axis.text.x = element_text(margin = margin(t = 16)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

# Create footer -----------------------------------------------------------


text_name <-   textGrob("Graphic by Michael Millett",
                        x = unit(0.02, "npc"),
                        gp = gpar(col = "white", fontfamily = f2, fontsize = 16),
                        hjust = 0)


text_source <- textGrob("Source: World Health Organization (WHO)",
                        x = unit(0.98, "npc"),
                        hjust = 1,
                        gp = gpar(col = "white", fontfamily = f2, fontsize = 16))


footer = grobTree(rectGrob(gp = gpar(fill = "#5B5E5F",
                                     lwd = 0)),
                  text_name,
                  text_source)

gridExtra::grid.arrange(plot, footer, heights = unit(c(0.94, 0.06), c('npc', 'npc')))


# Cleanup -----------------------------------------------------------------

rm(list = ls())

# Exported as SVG with dimensions: 1150 x 800