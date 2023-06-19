library(tidyverse)
library(rvest)
library(dplyr)

link <- "https://www.rosebikes.com/bikes/mtb"
homepage <- read_html(link)

bike_typ <- homepage %>%
  html_nodes(".catalog-category-bikes__category-title--level-2 .basic-headline__title") %>%
  html_text()

bike_subtyp <- homepage %>%
  html_nodes(".basic-headline--left .basic-headline__title") %>%
  html_text()

price <- homepage %>%
  html_nodes(".catalog-category-bikes__price-title") %>%
  html_text()
price <- gsub("[^0-9.]", "", price)

table <- data.frame(bike_typ, bike_subtyp, price)
