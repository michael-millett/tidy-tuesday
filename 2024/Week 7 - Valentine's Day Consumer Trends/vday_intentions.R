

# Script Name:      TidyTuesday: Valentines Day (Week 7)
# Author:           Michael Millett
# Date Created:     2/18/2024
# Email:            mcmillett.extra@gmail.com

# OBJECTIVE:
#   - Create a line graph showing the trend of US adults planning on
#       celebrating Valentine's Day

# Install pacman package ("package manager") if not currently installed -------

if (!require("pacman")) install.packages("pacman")
if (!require("signal")) install.packages("signal")

# Load packages

pacman::p_load(pacman, 
               tidyverse, 
               janitor,
               gridExtra,
               grid,
               showtext,
               ggtext)

# Load data

tuesdata <- tidytuesdayR::tt_load(x = 2024, week = 7)
spending <- tuesdata$historical_spending %>% 
  clean_names()

rm(tuesdata)


# Data Wrangling ----------------------------------------------------------


# OBJECTIVE:
#   - Interpolate values of percent celebrating between each observed year
#       to get a smooth line for the plot instead of jagged line


# METHOD:
#   - Use Piecewise Cubic Hermite Interpolating Polynomial method (PCHIP), so
#       that estimated values never overshoot their local minima/maxima values
#       for observed percent celebrating


# Create a new vector to have more x values to interpolate

new_year <- seq(from = 2010,       # min x value in spending
                to = 2022,         # max x value in spending
                length.out = 200)  # length of new vector from 2010 to 2022


# Create a new vector with the interpolated y values

y_interpolated <- signal::pchip(x = spending$year,
                                y = spending$percent_celebrating,
                                xi = new_year)

df <- tibble(x = new_year, 
             y = y_interpolated)


# Labels ------------------------------------------------------------------

points_above <- spending %>%
  filter(!year %in% c(2012, 2014, 2019, 2021)) %>% 
  select(
    year,
    percent = percent_celebrating
  )

points_below <- spending %>% 
  filter(year %in% c(2012, 2014, 2019, 2021)) %>% 
  select(
    year,
    percent = percent_celebrating
  )


# Load fonts --------------------------------------------------------------

font_add("open sans bold", "../../../../System/fonts/open_sans/static/OpenSans/OpenSans-Bold.ttf")
font_add("open sans", "../../../../System/fonts/open_sans/static/OpenSans/OpenSans-Regular.ttf")
font_add("aglet mono", "../../../../System/ggplot_fonts/Aglet-Mono-Features.otf")

showtext_auto()

f1 <- "open sans bold"
f2 <- "open sans"
f3 <- "aglet mono"

# Create Plot -------------------------------------------------------------

plot <- df %>% 
  ggplot(aes(x = x, y = y)) +
  
  geom_line(color = "#E15759",
            linewidth = 0.75) +
  
  geom_point(data = spending, 
             aes(x = year, y = percent_celebrating),
             size = 3,
             color = "white",
             fill = "#E15759",
             shape = 21) +
  
  geom_text(data = points_above,
            aes(x = year, y = percent, label = paste0(percent, "%")),
            nudge_x = 0.1,
            nudge_y = 0.7) +
  
  geom_text(data = points_below,
            aes(x = year, y = percent, label = paste0(percent, "%")),
            nudge_x = 0.4,
            nudge_y = -0.45) +
  
  scale_x_continuous(
    breaks = seq(from = 2010, to = 2022, by = 1),
    expand = c(0.025, 0.025)) +
  
  scale_y_continuous(
    labels = paste0(seq(from = 50, to = 62, by = 2), "%"),
    breaks = seq(from = 50, to = 62, by = 2),
    limits = c(50, 62),
    expand = c(0, 0)) +
  
  labs(
    title = "Valentine's Day Celebration Intentions Among US Adults",
    subtitle = "Percentage of US Adults Intending to Celebrate Valentine's Day (2010 - 2022)",
    y = NULL,
    x = NULL)+
  
  theme(
    
    plot.margin = margin(15, 15, 10, 15),
    
    plot.background = element_rect(color = "grey88"),
    
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "grey88"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88"),
    panel.grid.minor.y = element_blank(),
    
    plot.title = element_textbox(family = f1,
                                 size = 18,
                                 hjust = 0.5),
    
    plot.title.position = "plot",
    
    
    plot.subtitle = element_textbox(family = f2,
                                    size = 10,
                                    hjust = 0.5,
                                    margin = margin(b = 10),
                                    lineheight = 1.3),
    
    plot.caption = element_textbox(family = f2,
                                   size = 9),
    
    
    axis.title = element_blank(),
    axis.text = element_text(family = f2, size = 11),
    axis.text.y = element_text(hjust = 0, margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 8)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )


# Create footer -----------------------------------------------------------


text_name <-   textGrob("Graphic by Michael Millett",
                        x = unit(0.02, "npc"),
                        gp = gpar(col = "white", fontfamily = f2, fontsize = 10),
                        hjust = 0)


text_source <- textGrob("Source: National Retail Federation (NRF)",
                        x = unit(0.98, "npc"),
                        hjust = 1,
                        gp = gpar(col = "white", fontfamily = f2, fontsize = 10))


footer = grobTree(rectGrob(gp = gpar(fill = "#5B5E5F",
                                     lwd = 0)),
                  text_name,
                  text_source)

grid.arrange(plot, footer, heights = unit(c(0.94, 0.06), c('npc', 'npc')))


# Cleanup -----------------------------------------------------------------

rm(list = ls())
graphics.off()
p_unload(all)
