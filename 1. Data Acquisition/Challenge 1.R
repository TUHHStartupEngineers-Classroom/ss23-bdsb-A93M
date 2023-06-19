library(jsonlite)
library(ggplot2)

# API endpoint URL
url <- "https://api.coindesk.com/v1/bpi/currentprice.json"

# Send GET request and parse JSON response
response <- jsonlite::fromJSON(url)

# Extract relevant data
bpi <- response$bpi

# Create a data frame from the extracted data
bpi_df <- data.frame(currency = names(bpi), price = unlist(bpi), stringsAsFactors = FALSE)

# Create the plot
plot <- ggplot(bpi_df, aes(x = currency, y = price)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Current Bitcoin Prices",
       x = "Currency", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(plot)
