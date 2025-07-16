
# Script Name:      TidyTuesday: Weekly US Gas Prices (Week 26 / 2025)
# Author:           Michael Millett
# Date Created:     7/9/2025

# OBJECTIVE:
#   - Create a line graph that shows the history of gas & diesel prices


# LOAD PACKAGES -----------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(grid)
library(showtext)
library(ggtext)
library(cowplot)
library(svglite)

# LOAD DATA ---------------------------------------------------------------

if (!require("tidytuesdayR")) install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load(2025, week = 26)

df <- tuesdata$weekly_gas_prices

rm(tuesdata)


# EXTRACT DATA FOR GRAPH --------------------------------------------------

df1 <- df %>% 
  filter(fuel == "diesel" | grade == "regular" & formulation == "all") %>% 
  mutate(fuel = str_to_title(fuel))

# LOAD FONTS --------------------------------------------------------------

font_add("open sans bold", "../../../../System/fonts/open_sans/static/OpenSans/OpenSans-Bold.ttf")

font_add("open sans regular", "../../../../System/fonts/open_sans/static/OpenSans/OpenSans-Regular.ttf")

showtext_auto()

f1 <- "open sans bold"
f2 <- "open sans regular"

# CREATE PLOT -------------------------------------------------------------

colors <- c("#3CA951", "#333333")


plot <- ggplot() +
  
  geom_line(data = df1, 
            aes(x = date, y = price, colour = fuel),
            linewidth = 0.6) +
  
  scale_x_date(
    breaks = seq(from = as_date("1990-01-01"), to = as_date("2025-01-01"), by = "5 year"),
    labels = c("1990", "", "2000", "", "2010", "", "2020", ""),
    expand = c(0.075, 0)) +
  
  scale_y_continuous(
    breaks = seq(from = 1, to = 6, by = 1),
    limits = c(0.75, 6),
    expand = c(0, 0.25)) +
  
  scale_color_manual(values = colors) +
  
  guides(color = guide_legend(override.aes = list(linewidth = 5))) +
  
  labs(
    title = "US Weekly Fuel Prices",
    subtitle = "Weekly sampled fuel prices in US (1990–2025)",
    y = NULL,
    x = NULL) +
  
  theme(
    
    plot.margin = margin(20, 30, 30, 40),
    plot.background = element_rect(color = "grey88"),
    
    
    # Panels
    
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "grey88"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor.y = element_blank(),
    
    
    # Titles and Captions
    
    plot.title = element_textbox(family = f1,
                                 size = 48,
                                 hjust = 0.5),
    
    
    plot.title.position = "plot",
    
    
    plot.subtitle = element_textbox(family = f2,
                                    size = 20,
                                    hjust = 0.5,
                                    margin = margin(t = 5, b = 20),
                                    lineheight = 1.3),
    
    plot.caption = element_textbox(family = f2,
                                   size = 14,
                                   margin = margin(t = 20, b = 5),
                                   color = "#B8B8B8"),
    
    # Legend 
    
    legend.position = c(0.25, 0.6),
    legend.key = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    legend.text = element_text(family = f2, size = 16),
    legend.key.size = unit(2, "lines"),
    legend.key.width = unit(0.75, "lines"),
    legend.text.align = 0,
    
    
    # Axis
    
    axis.title = element_blank(),
    axis.text = element_text(family = f2, size = 18),
    axis.text.y = element_text(hjust = 0, margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 16)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
    
  )


# ADD CUSTOM Y-AXIS LABEL -------------------------------------------------


plot <- ggdraw(plot) +
  draw_label("Price ($/gal)", 
             x = 0.07, 
             y = 0.83,
             size = 10.5,
             fontfamily = f2,
             color = "#444444"
             )

# CREATE FOOTER -----------------------------------------------------------


text_name <-   textGrob("Graphic by Michael Millett",
                        x = unit(0.02, "npc"),
                        gp = gpar(col = "white", fontfamily = f2, fontsize = 16),
                        hjust = 0)


text_source <- textGrob("Source: US Energy Information Administration (EIA)",
                        x = unit(0.98, "npc"),
                        hjust = 1,
                        gp = gpar(col = "white", fontfamily = f2, fontsize = 16))


footer = grobTree(rectGrob(gp = gpar(fill = "#5B5E5F",
                                     lwd = 0)),
                  text_name,
                  text_source)

gridExtra::grid.arrange(plot, footer, heights = unit(c(0.94, 0.06), c('npc', 'npc')))


# SAVE FILE ---------------------------------------------------------------

ggsave("fuel_prices.svg",
       plot = plot,
       width = 3000,
       height = 2400,
       units = "px")

rm(list = ls())