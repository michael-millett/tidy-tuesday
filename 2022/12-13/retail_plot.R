
# Script Name:      tidytuesday week 50: retail data
# Purpose:          This is for the TidyTuesday Script
# Author:           Michael Millett
# Date Created:     2022-12-16

# Author: Michael Millett, 2022
# Email:  mcmillett.extra@gmail.com

# Load packages needed for analysis
pacman::p_load(pacman, tidyverse, janitor, ggtext, magrittr, broom,
               rio, lubridate, showtext, RColorBrewer)

# Add custom fonts from Google (open source)
font_add_google("inter", family = "Inter")
font_add_google("chivo", family = "ChivoMono")
font_add_google("dm mono", family = "DM Mono")

# These fonts you will have to download off google fonts directly and import from system
#font_add("inter_tight", regular = "../../../system_files/fonts/Inter_Tight/static/InterTight-Regular.ttf")
#font_add("inter_tight_bold", regular = "../../../system_files/fonts/Inter_Tight/static/InterTight-Bold.ttf")


showtext_auto() # to enable custom fonts in ggplot2

# IMPORT FRED DATA --------------------------------------------------------

# Downloaded from FRED and datasets included in github repository

# price_index <- read_csv("source_data/fred_cpi_2019_2022.csv") %>%
#   as_tibble() %>%
#   clean_names() %>%
#   rename(cpi_change_yoy = cpiaucsl_pc1) %>%
#   mutate(date = mdy(date))
#
# price_index %>% saveRDS("fred_cpi_data.rds")
#
# consumer_expenditures <- read_csv("source_data/fred_pce_2019_2022.csv") %>%
#   as_tibble() %>%
#   clean_names() %>%
#   rename(pce_change_yoy = pce_pc1)
#
# price_index %>% saveRDS("data/fred_cpi_data.rds")
# consumer_expenditures %>% saveRDS("data/fred_pce_data.rds")
#
# fred_data <- left_join(price_index, consumer_expenditures, by = "date")
#
# fred_data %>% saveRDS("data/fred_data.rds")
#
# rm(consumer_expenditures, price_index)


# LOAD DATA FOR ANALYSIS --------------------------------------------------

df <- import("data/retail_data.rds")
fred_data <- import("data/fred_data.rds")

df %<>%
  filter(change_yoy != "S") %>%
  rowid_to_column() %>%
  unite(col = "ym_date",
        c("year", "month"),
        sep = "-",
        remove = FALSE) %>%
  mutate(change_yoy = as.numeric(change_yoy),
         ym_date = ym(ym_date)) %>%
  mutate(across(c("fips", "state_abbr", "naics", "subsector"), as_factor),
         month = month(ym_date, label = TRUE, abbr = TRUE))

df <- left_join(df, fred_data, by = c("ym_date" = "date"))

####
## Optional - save the file if you want to keep it for reference
####

# df %>% saveRDS("data/cpi_retail_data.rds")

rm(fred_data) # remove fred_data to keep workspace tidy

####
## Reorient Data frame
####

# Pivot longer and only keep USA total
#   - this is so we can easily color/fill by the different rates
#   - we'll put all the rates in their own column and their labels in another

df <- df %>%
  select(state_abbr, subsector, ym_date,
         change_yoy, cpi_change_yoy, pce_change_yoy) %>%
  rename(retail_trade_yoy = change_yoy) %>%
  filter(state_abbr == "USA",
         subsector == "total") %>%
  pivot_longer(cols = c("retail_trade_yoy",
                        "cpi_change_yoy",
                        "pce_change_yoy"),
               names_to = "economic_indicator",
               values_to = "yoy_change")

# MAKING THE PLOT ---------------------------------------------------------

# Custom Labels
x_labels <- c("2019", "2020", "2021", "2022") # years for the yoy data
y_labels <- c("-20%", "0%", "20%", "40%")     # percentages on y axis

# Theme
custom_theme <- theme(plot.margin = margin(c(15, 10, 30, 10)),
                      panel.background = element_rect(fill = "grey96", size = 0),
                      plot.background = element_rect(fill = "grey96", size = 0),
                      panel.grid.major = element_line(color = "grey83"),
                      panel.grid.minor = element_line(color = "grey83"),
                      axis.ticks = element_line(color = "grey83"),
                      plot.title = element_text(size = 19, family = "inter_tight_bold"),
                      plot.subtitle = element_text(size = 11, family = "inter_tight"),
                      axis.text = element_text(size = 10, family = "ChivoMono"),
                      legend.position = c(0.29, 0.76),
                      legend.key = element_rect(fill = "transparent"),
                      legend.background = element_rect(fill = "transparent"),
                      legend.title = element_blank(),
                      legend.text = element_text(family = "ChivoMono", size = 10),
                      plot.caption = element_text(family = "DM Mono", size = 8, vjust = -8))


# Colors for the lines created as character vector for hex codes
#   - from RColorBrewer package palettes
colors <- c(
  brewer.pal(11, "Spectral")[2],    # for Consumer Price Index
  brewer.pal(11, "BrBG")[10],       # for Personal Consumer Expenditure
  brewer.pal(11, "Spectral")[10])   # for Retail Trade

# Creating the Plot

df %>%
  ggplot(data = ., aes(x = ym_date,
                       y = yoy_change,
                       color = economic_indicator,
                       fill  = economic_indicator)) +
  geom_line(size = 1.05, alpha = 0.7) +
  scale_y_continuous(limits = c(-25, 55),
                     breaks = seq(-20, 50, 20),
                     minor_breaks = NULL,
                     labels = y_labels) +
  scale_x_date(breaks = as.Date(c("2019-01-15",
                                  "2020-01-01",
                                  "2021-01-01",
                                  "2022-01-01")),
               minor_breaks = NULL,
               labels = x_labels) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.3,
             size = 1) +
  xlab(NULL) +
  ylab(NULL) +
  labs(title = "Consumer Spending Activity in the US since 2019",
       subtitle = "Indexes Measured as Percent Change in Comparison to the Previous Year",
       caption = "Source: Federal Reserve Economic Data (FRED) & US Census Bureau | Graphic by Michael Millett") +
  scale_color_manual(values = colors,
                     labels = c("Consumer Price Index",
                                "Personal Consumer Expenditure",
                                "Retail Trade")) +
  custom_theme

# Save the Plot as a PDF and then export to PNG to maintain font scaling

# ggsave("yoy_retail_plot.pdf",
#        last_plot(),
#        height = 5,
#        width = 7.25,
#        units = "in",
#        device = "pdf",
#        dpi = 600)

####
## Clean up
####

# p_unload(all)
# graphics.off()
# rm(list=ls())
