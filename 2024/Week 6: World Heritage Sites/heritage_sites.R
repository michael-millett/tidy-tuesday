
# Script Name:      World Heritage Sites: Tidy Tuesday Week 6 (2024)
# Author:           Michael Millett
# Date Created:     2024-02-05
# Email:            mcmillett.extra@gmail.com


# OBJECTIVE:
#   - Use the data  to create a graphic visualization in a similar style
#       of graphs featured in The Economist magezine


# Install pacman package ("package manager") if not installed -------------

if (!require("pacman")) install.packages("pacman")


# Load packages -----------------------------------------------------------

pacman::p_load(pacman,
               tidyverse,
               janitor,
               showtext,
               ggtext)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 6)

df <- tuesdata$heritage %>%
  pivot_longer(cols = c(`2004`, `2022`),
               names_to  = "year",
               values_to = "n_sites") %>%
  mutate(country = as_factor(country)) %>%
  mutate(country = fct_relevel(country, c("Norway", "Denmark", "Sweden")))

rm(tuesdata)


# Load fonts --------------------------------------------------------------

# Font Used is Asap Condensed from Google Fonts

font_add("asap regular", "../../../../system_files/ggplot_fonts/AsapCondensed-Regular.ttf")
font_add("asap semibold", "../../../../system_files/ggplot_fonts/AsapCondensed-SemiBold.ttf")
font_add("asap medium", "../../../../system_files/ggplot_fonts/AsapCondensed-Medium.ttf")

showtext_auto()

f1 <- "asap regular"
f2 <- "asap semibold"
f3 <- "asap medium"


# Plot --------------------------------------------------------------------

ggplot(data = df, aes(x = n_sites, y = country, fill = year)) +

  geom_col(width = 0.75,
           position = position_dodge2(width = 0.01)) +

  scale_fill_manual(values = c("#3EBCD2", "#006ba2")) +

  scale_y_discrete(expand = c(0.175, 0.175)) +

  scale_x_continuous(position = "top") +

  labs(
    title = "Number of World Heritage Sites",
    subtitle = paste("World Heritage Sites are designated by UNESCO for having  \ncultural, ",
                     "historical, scientific or other forms of significance",
                     sep = ""),
    caption = "Souce: UNESCO World Heritage Sites | Graphic by Michael Millett"
  ) +

  coord_fixed(clip = "off") +

  theme(
    aspect.ratio = 1/1,

    plot.margin = margin(10, 6, 5, 6),

    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "grey88"),
    panel.grid.minor.x = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),

    plot.title = element_textbox(family = f2,
                                 size = 17,
                                 hjust = -1.8,
                                 margin = margin(b = 7)),

    plot.subtitle = element_textbox(family = f1,
                                    size = 12,
                                    hjust = 1.1,
                                    margin = margin(b = 12),
                                    lineheight = 1.2),

    plot.caption = element_textbox(family = f1,
                                   size = 9,
                                   hjust = -5.15),


    axis.title = element_blank(),
    axis.text = element_text(family = f3, size = 12),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -2)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),


    legend.position = "top",
    legend.text = element_text(family = f1, size = 10, margin = margin(t = -2, b = -2)),
    legend.title = element_blank(),
    legend.key.height = unit(0, "points"),
    legend.justification = -0.4,
    legend.margin = margin()
  )

# Plot exported as PNG 399 x 455
