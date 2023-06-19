library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)

# 1.0 DATA IMPORT ----

#1.1 Patent DATA ----
col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "/Users/anusc/Documents/GitHub/ss23-bdsb-A93M/02 Data Wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

# 1.2 Assignee DATA ----

col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "/Users/anusc/Documents/GitHub/ss23-bdsb-A93M/02 Data Wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

# 1.3 Patent-Assignee DATA ----
col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "/Users/anusc/Documents/GitHub/ss23-bdsb-A93M/02 Data Wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

# 1.4 USPC DATA
col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "/Users/anusc/Documents/GitHub/ss23-bdsb-A93M/02 Data Wrangling/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

# 2.1 Patent Data ----
class(patent_tbl)
setDT(patent_tbl)
class(patent_tbl)

# 2.2 Assignee DATA ----
setDT(assignee_tbl)

# 2.3 Patent Assignee DATA ----
setDT(patent_assignee_tbl)

# 2.4 USPC DATA ----
setDT(uspc_tbl)

# 3.0 DATA WRANGLING ----

# 3.1 Joining / Merging DATA Question 1----

tic()
combined_data1 <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                      by.x = "id", by.y = "assignee_id")
toc()

# 3.2 Filter based on "type 2" type
combined_data1_filtered <- combined_data1 %>%
  filter(type == "2")

# 4.1 Answer to Question 1

# Count the number of patents 
patent_counts <- aggregate(patent_id ~ organization, combined_data1_filtered, FUN = length)

# Sort the organizations 
sorted_counts <- patent_counts[order(-patent_counts$patent_id), ]

# Display the top 10 organizations 
top_10_companies <- head(sorted_counts, 10)
print(top_10_companies)

# 4.2 Answer to Question 2

# 4.2.1 Joining / Merging DATA Question 2----

# Convert date column in patent_tbl 
patent_tbl$date <- format(patent_tbl$date, "%Y-%m")

# Filter patent_tbl to include only August 2014
patent_aug2014 <- patent_tbl[format(patent_tbl$date, "%Y-%m") == "2014-08"]

tic()
combined_data2 <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                        by.x = "id", by.y = "assignee_id")
combined_data2 <- merge(x = combined_data2, y = patent_tbl, 
                        by.x = "id", by.y = "date")
toc()

combined_data2_filtered <- combined_data2 %>%
  filter(type == "2")

# Count the number of patents in August 2014
patent_counts_aug2014 <- aggregate(patent_id ~ organization, data = patent_aug2014, FUN = length)

# Sort the organizations by the number of patents
sorted_counts_aug2014 <- patent_counts_aug2014[order(-patent_counts_aug2014$patent_id) ]

# Display the top 10 organizations for August 2014
top_10_companies_aug2014 <- head(sorted_counts_aug2014, 10)
print(top_10_companies_aug2014)

# 4.3 Answer to Question 3

# Get the top 10 companies
top_10_company_ids <- top_10_companies$organization

# Filter combined_data2 to include only the top 10 companies
combined_data2_top_10 <- combined_data2_filtered %>%
  filter(organization %in% top_10_company_ids)

# Join with uspc_tbl 
combined_data2_top_10 <- merge(x = combined_data2_top_10, y = uspc_tbl,
                               by = "patent_id")

# Count the occurrences 
main_class_counts <- combined_data2_top_10 %>%
  count(mainclass_id)

# Sort the main classes 
sorted_main_classes <- main_class_counts[order(-main_class_counts$n), ]

# Get the top 5 USPTO 
top_5_main_classes <- head(sorted_main_classes, 5)
print(top_5_main_classes)