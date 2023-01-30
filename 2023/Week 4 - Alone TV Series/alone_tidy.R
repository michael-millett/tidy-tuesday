
# Script Name:      alone_tidy.R
# Purpose:          Alone TV Series Average Days Lasted
# Author:           Michael Millett
# Date Created:     2023-01-28
# Email:            mcmillett.extra@gmail.com

# OBJECTIVE:
#   - Show the average number of days lasted for each rank spanning 9 seasons

# Load packages
pacman::p_load(pacman,      # for loading packages
               tidyverse,   # for TidyVerse syntax and packages
               showtext,    # for loading custom fonts from system
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


# LABELS ------------------------------------------------------------------

# Storing fonts as objects for easy reference when making plot
f1 <- "myriad_pro"
f2 <- "roboto"
f3 <- "aglet_mono"

# Y-axis labels
y_labels <- c("Day 0", seq(10, 80, 10))
x_labels <- c("10th", "9th", "8th", "7th", "6th", "5th", "4th", "3rd", "2nd",
              "1st")

# LOAD DATA ---------------------------------------------------------------

# Load the TidyTuesday data
tuesdata <- tidytuesdayR::tt_load(2023, week = 4)

df <- tuesdata$survivalists %>%
  group_by(result) %>%
  summarize(avg_days_lasted = mean(days_lasted)) %>%
  round(0) %>%
  mutate(result = as_factor(result))

df <- df %>%
  mutate(result = fct_relevel(df$result, rev))

rm(tuesdata) # remove tuesdata to keep environment clean

# PLOT --------------------------------------------------------------------

df %>%
  ggplot(aes(x = result, y = avg_days_lasted, group = 1)) +
  geom_hline(yintercept = c(30, 60),
             linetype = "longdash",
             color = "#AA4A44",
             linewidth = 1,
             alpha = 0.9) +
  geom_line(color = "grey20", linewidth = 0.7) +
  geom_point(fill = "grey",
             color = "grey20",
             size = 5,
             shape = 21) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, 10),
    minor_breaks = NULL,
    labels = y_labels) +
  scale_x_discrete(
    labels = x_labels
  ) +
  xlab(label = "Place Contestant Finished (in their season)") +
  ylab(NULL) +
  labs(title = "Alone (TV Series) - How Long Do Contestants Last?",
       subtitle = "Average number of days until a contestant tapped out (9 seasons total)",
       caption = "Source: Alone Data Package | Graphic by Michael Millett") +
  annotate(
    geom = "text",
    label = "1 Month ",
    x = 5.05,
    y = 32.25,
    size = 4,
    color = "#AA4A44",
    family = f3
  ) +
  annotate(
    geom = "text",
    label = "2 Months",
    x = 7.3,
    y = 62.25,
    size = 4,
    color = "#AA4A44",
    family = f3
  ) +
  theme(
    plot.margin = margin(c(20, 40, 35, 20)),
    panel.background = element_rect(fill = "grey98", linewidth = 0),
    plot.background = element_rect(fill = "grey98", linewidth = 0),
    panel.grid.major.y = element_line(color = "grey78", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 24, family = f1, hjust = 0.5),
    plot.subtitle = element_text(size = 13, family = f2, color = "grey40", hjust = 0.5),
    axis.text.x = element_text(size = 13, family = f3),
    axis.text.y = element_text(size = 13, family = f3),
    axis.title.x = element_text(size = 11, family = f3, vjust = -2, color = "grey20"),
    plot.caption = element_text(size = 10, family = f3, vjust = -7, color = "grey20")
  )

# Plot exported as pdf (to maintain scaling) 10.05 x 7.24
