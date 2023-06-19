library(tidyverse)
library(ggplot2)
library(maps)

covid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

covid_data <- covid_data %>%
  mutate(mortality_rate = total_deaths / population)

plot_data <- covid_data %>%
  filter(!is.na(mortality_rate)) %>%
  select(location, mortality_rate)

world <- map_data("world")

plot_data %>%
  ggplot() +
  geom_map(aes(map_id = location, fill = mortality_rate), map = world) +
  expand_limits(x = world$long, y = world$lat) +
  scale_fill_gradient(low = "red", high = "black") +
  labs(fill = "Mortality Rate", title = "Distribution of Mortality Rate") +
  theme_minimal()
