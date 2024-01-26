
# Script Name:      Educational Attainment of Young People.R
# Purpose:          Studying factors which influence education outcomes in England
# Author:           Michael Millett
# Date Created:     2024-01-26
# Email:            mcmillett.extra@gmail.com

# OBJECTIVE:
#   - Recreate the graphic published on the ONS website

# Link: https://www.ons.gov.uk/peoplepopulationandcommunity/educationandchildcare/articles/whydochildrenandyoungpeopleinsmallertownsdobetteracademicallythanthoseinlargertowns/2023-07-25


# LOAD DATA ---------------------------------------------------------------

pacman::p_load(
  pacman,
  tidyverse,
  rio,
  janitor,
  skimr,
  ggplot2,
  showtext,    # for loading custom fonts from system
  ggtext       # for extra text specification in plot
)

ons_link <- "https://www.ons.gov.uk/visualisations/dvc2651b/fig3/datadownload.xlsx"

httr::GET(url = ons_link,
          httr::write_disk(tempfile <- tempfile(fileext = ".xlsx")))

df1 <- readxl::read_excel(tempfile, skip = 3) %>%
  clean_names() %>%
  rename(
    ed = educational_attainment_score,
    income = income_deprivation_score
  )

rm(ons_link, tempfile)

# LOADING CUSTOM FONTS ----------------------------------------------------

# These are the fonts used (NOTE: only Roboto is open-source)
#   - Title:      Myriad-Pro      (Adobe fonts)
#   - Subtitle:   Roboto          (Google fonts)
#   - Axis:       Roboto          (Google fonts)

font_add(
  "myriad_pro",
  regular = "../../../../system_files/ggplot_fonts/MyriadPro-Bold.otf")

font_add(
  "roboto",
  regular = "../../../../system_files/ggplot_fonts/Roboto-Regular.ttf")

showtext_auto() # function to enable the loaded fonts

# Storing fonts as objects for easy reference when making plot
f1 <- "myriad_pro"
f2 <- "roboto"


# Graphic ----

ggplot(data = df1,
       mapping = aes(x = ed, y = income)) +

  geom_vline(
    xintercept = c(0, 0),
    color = "darkgrey",
    linewidth = 0.75,
    alpha = 0.85
  ) +

  geom_point(color = "#004f83",
             alpha = 0.35,
             size = 3.5) +

  scale_y_continuous(
    limits = c(0.65, 1.0),
    expand = c(0,0),
    breaks = seq(0.65, 1.0, 0.05),
    labels = scales::number_format(accuracy = 0.01)) +

  xlab(label = "Educational attainment score") +

  ylab(label = NULL) +

  labs(
    title = "Educational Attainment of Students in England",
    caption = "Source: Office of National Statistics (ONS) | Graphic by Michael Millett") +

  geom_segment(
    x    = -10.4,
    xend = -10.4,
    y    = 0.93,
    yend = 0.99,
    linewidth = 0.7,
    lineend  = "round",
    linejoin = "round",
    size = 0.75,
    color = "darkgrey",
    arrow = arrow(
      angle = 25,
      length = unit(0.25, "cm"),
      type = "open")
  ) +

  geom_segment(
    x    = 8.5,
    xend = 11.5,
    y    = 0.67,
    yend = 0.67,
    size = 0.75,
    linewidth = 0.7,
    lineend  = "round",
    linejoin = "round",
    color = "darkgrey",
    arrow = arrow(
      angle = 25,
      length = unit(0.25, "cm"),
      type = "open")
  ) +

  annotate(
    geom = "text",
    label = "Lower deprivation",
    x = -7.5,
    y = 0.98,
    color = "grey20",
    family = f2
  ) +

  annotate(
    geom = "text",
    label = "Higher attainment",
    x = 6,
    y = 0.671,
    color = "grey20",
    family = f2
  ) +

  annotate(
    geom = "text",
    label = "Income deprivation score",
    x = Inf,
    y = Inf,
    vjust = -2,
    hjust = 4,
    color = "grey40",
    family = f2
  ) +

  coord_cartesian(clip = "off") +

  theme(
    plot.margin = margin(c(50, 40, 25, 25)),
    panel.background = element_rect(fill = "grey98", linewidth = 0),
    plot.background = element_rect(fill = "grey98", linewidth = 0),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.30),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.30),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 24, family = f1, hjust = 0.5, vjust = 7),
    plot.subtitle = element_text(size = 13, family = f2, color = "grey40", hjust = 0.5),
    axis.text.x = element_text(size = 11, family = f2, vjust = -1.5),
    axis.text.y = element_text(size = 11, family = f2),
    axis.title.x = element_text(size = 11, family = f2, vjust = -3, hjust = 0.975, color = "grey37"),
    plot.caption = element_text(size = 8, family = f2, vjust = -10, hjust = 0.5, color = "grey50")
  )

# Plot exported as PNG 754 pixels by 665

p_unload(all)
graphics.off()
rm(list=ls())
