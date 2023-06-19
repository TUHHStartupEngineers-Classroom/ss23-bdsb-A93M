# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)

# Import files
bikes <- read_excel("/Users/anusc/Documents/GitHub/ss23-bdsb-A93M/0. Intro to Tidyverse/00_data/01_raw_data/bikes.xlsx")
orderlines <- read_excel("/Users/anusc/Documents/GitHub/ss23-bdsb-A93M/0. Intro to Tidyverse/00_data/01_raw_data/orderlines.xlsx")
bikeshops <- read_excel("/Users/anusc/Documents/GitHub/ss23-bdsb-A93M/0. Intro to Tidyverse/00_data/01_raw_data/bikeshops.xlsx")

# Joining Data
bike_orderlines_joined <- left_join(orderlines, bikes, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops, by = c("customer.id" = "bikeshop.id"))

# Wrangling Data
bike_orderlines_wrangled <- bike_orderlines_joined %>%
  separate(col = location, into = c("city", "state"), sep = ",") %>%
  mutate(revenue = price * quantity)

# Task 1: Analyze Sales by Location (State)
sales_by_state <- bike_orderlines_wrangled %>%
  group_by(state) %>%
  summarize(total_revenue = sum(revenue)) %>%
  arrange(desc(total_revenue))

# Bar plot of Sales by State
ggplot(data = sales_by_state, aes(x = state, y = total_revenue)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = scales::dollar(total_revenue, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")),
             vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")) +
  labs(
    title = "Sales by Location (State)",
    x = "State",
    y = "Total Revenue"
  )

# Find State with the Highest Revenue
highest_revenue_state <- sales_by_state$state[1]

# Task 2: Analyze Sales by Location and Year (Facet Wrap)
sales_by_year_loc <- bike_orderlines_wrangled %>%
  mutate(year = year(order.date)) %>%
  group_by(state, year) %>%
  summarize(total_revenue = sum(revenue), .groups = "drop") %>%
  complete(state, year, fill = list(total_revenue = 0)) %>%
  mutate(sales_text = scales::dollar(total_revenue, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"))

# Bar plot of Sales by Year and Location (Facet Wrap)
ggplot(data = sales_by_year_loc, aes(x = year, y = total_revenue, fill = state)) +
  geom_col() +
  geom_label(aes(label = sales_text), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")) +
  labs(
    title = "Revenue by Year and Location",
    subtitle = "Highest Revenue",
    fill = "State",
    x = "Year",
    y = "Total Revenue"
  ) +
  facet_wrap(~ state, ncol = 4)

