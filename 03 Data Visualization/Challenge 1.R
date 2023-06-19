library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)


covid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")


covid_daily_continent <- covid_data %>%
  filter(!is.na(continent)) %>%
  group_by(date, continent) %>%
  summarize(daily_cases = sum(new_cases, na.rm = TRUE)) %>%
  filter(continent %in% c("Asia", "Africa", "Europe", "North America", "South America", "Oceania"))


covid_daily_continent$date <- as.Date(covid_daily_continent$date)


custom_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FF00FF", "#FFFF00", "#00FFFF")


ggplot(covid_daily_continent, aes(x = date, y = daily_cases, color = continent, group = continent)) +
  geom_line() +
  scale_x_date(labels = date_format("%b %Y"), date_breaks = "6 month") +
  scale_y_continuous(labels = comma) +
  labs(x = "Date", y = "Daily Cases", color = "Continent") +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(title = "Continent"))
