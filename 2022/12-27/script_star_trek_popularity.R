# Script Name:      Popularity of Star Trek Franchise
# Purpose:          TidyTuesday Week 52 Dataset
# Author:           Michael Millett
# Date Created:     2023-JAN-03
# Email:            mcmillett.extra@gmail.com

# Load packages needed for analysis

pacman::p_load(pacman, tidyverse, janitor, skimr, ggtext, magrittr, broom,
               rio, lubridate)

####
## NOTE ABOUT FONTS
####

# I use the showtext package to load fonts I have from Adobe Fonts
#   - Neue Haas Grotesk Display Bold(title)
#   - Aglet Mono Regular (caption and axis labels)

#   - Inter (subtitle and labels--Google Fonts)

# p_load(showtext)
#
# font_add(family = "inter",
#          regular = "../../../system_files/fonts/Inter-Edited/Inter-Regular.ttf")
# font_add(family = "neue_haas",
#          regular = "../../../system_files/fonts/otf_features/NeueHaasDisplay-Bold-Features.otf")
# font_add(family = "aglet",
#          regular = "../../../system_files/fonts/otf_features/Aglet-Mono-Features.otf")
#
# showtext_auto()

# Import data set(s)
df <- rtrek::stBooks

df <- df %>%
  mutate(date = ymd(date),
         year_published = year(date),
         month_published = month(date))

df <- df %>%
  group_by(year_published) %>%
  count(.) %>%
  ungroup() %>%
  rename(count_books_published = n)

####
## Create Custom Axis Labels
####

x_labels <- c("1970", "'80", "'90", "2000", "'10")
y_labels <- c("0", "10", "20", "30", "40", "50")

####
## Create Key Dates Data Frame for geom_points in Plot
####

df_key_dates <- df %>%
  filter(year_published %in% c(1987, 1993, 1995, 2001, 2009)) %>%
  add_row(year_published = 1966,
          count_books_published = 1) %>%
  arrange(year_published)

####
## Make the Line Graph Plot
####

ggplot() +
  geom_line(data = df,
            aes(x = year_published, y = count_books_published),
            size = 1,
            color = "grey20",
            alpha = 0.8) +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, 10),
                     minor_breaks = NULL,
                     labels = y_labels) +
  scale_x_continuous(limits = c(1965, 2020),
                    breaks = seq(1970, 2010, 10),
                    labels = x_labels) +
  geom_point(data = df_key_dates,
             aes(x = year_published,
                 y = count_books_published),
             size = 5,
             color = "violetred3",
             alpha = 0.8) +
  geom_segment(aes(x = 1966.15,
                   y = 2,
                   xend = 1966.5,
                   yend = 5),
               size = 1) +
  geom_segment(aes(x = 1986.5,
                   y = 8.6,
                   xend = 1984.5,
                   yend = 12),
               size = 1) +
  geom_segment(aes(x = 1992.5,
                   y = 20.5,
                   xend = 1991.2,
                   yend = 22),
               size = 1) +
  geom_segment(aes(x = 1994.8,
                   y = 25,
                   xend = 1993,
                   yend = 29),
               size = 1) +
  geom_segment(aes(x = 2000.2,
                   y = 37,
                   xend = 1997,
                   yend = 39),
               size = 1) +
  geom_segment(aes(x = 2009.4,
                   y = 17.8,
                   xend = 2010.3,
                   yend = 20.6),
               size = 1) +
  annotate(geom = "label",
           x = 1970,
           y = 8,
           label = "The Original Series Debuts\nSeptember 8th, 1966",
           family = "inter") +
  annotate(geom = "label",
           x = 1980.5,
           y = 15,
           label = "Star Trek: The Next Generation\nSeptember 28th, 1987",
           family = "inter") +
  annotate(geom = "label",
           x = 1985.5,
           y = 23,
           label = "Star Trek: Deep Space 9\nJanuary 4th, 1993",
           family = "inter") +
  annotate(geom = "label",
           x = 1989.5,
           y = 32,
           label = "Star Trek: Voyager\nJanuary 16th, 1995",
           family = "inter") +
  annotate(geom = "label",
           x = 1995,
           y = 42,
           label = "Star Trek: Enterprise\nSeptember 26th, 2001",
           family = "inter") +
  annotate(geom = "label",
           x = 2014.5,
           y = 23.5,
           label = "Star Trek Movie (JJ Abrams)\nMay 8th, 2009",
           family = "inter") +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Star Trek Franchise Popularity",
       subtitle = "As a Measure of Total Books Published Per Year",
       caption = "Source: rtrek package | Graphic by Michael Millett") +
  theme(
    plot.margin = margin(c(20, 15, 30, 15)),
    panel.background = element_rect(fill = "grey96", size = 0),
    plot.background = element_rect(fill = "grey96", size = 0),
    panel.grid.major = element_line(color = "grey83"),
    panel.grid.minor = element_line(color = "grey83"),
    axis.ticks = element_line(color = "grey83"),
    plot.title = element_text(size = 32, family = "neue_haas", vjust = 1, hjust = 0.5),
    plot.subtitle = element_text(size = 13, family = "inter", color = "grey40", vjust = 1.5, hjust = 0.5),
    axis.text = element_text(size = 13, family = "aglet"),
    plot.caption = element_text(family = "aglet", size = 11, vjust = -4)
  )

####
## EXPORT PLOT
####

# plot exported (using cairo) as PDF with dimensions: 11.35 x 6.98 inches
# to maintain font scaling

####
## CLEANUP
####

# p_unload(all)
# graphics.off()
# rm(list=ls())
