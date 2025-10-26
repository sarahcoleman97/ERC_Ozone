#### PARSING BULK, COUNT DATA #####

# Date counts from website # 

# https://www.tceq.texas.gov/airquality/monops/historical-ozone-days

library(tidyverse)
library(rvest)
library(rlist)

url <- "https://www.tceq.texas.gov/airquality/monops/historical-ozone-days"
page <- read_html(url) #Creates an html document from URL
table <- html_table(page, fill = TRUE) #Parses tables into data frames
counts.raw <- table[[1]] %>% 
  filter(Year <= 2023)

prefix <- "../data-raw/oad/"
write.csv(counts.raw,
          paste0(prefix,'raw_aggregate_counts.csv'))

#### PARSING ACTUAL DATES #### 

# Define the URL
url <- "https://www.tceq.texas.gov/airquality/monops/historical-ozone-days"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Find the tabbed area
tab_area <- html_node(webpage, "#myTabContent")

# Extract tab content
tabs <- html_elements(tab_area, "div.row")

# Function to extract dates from a string
extract_dates <- function(text) {
  dates <- str_extract_all(text, "\\d{2}/\\d{2}/\\d{2}")
  dates <- unlist(lapply(dates, function(x) x[!is.na(x)]))
  return(dates)
}

# Lists to store tab names and dates
tab_names <- character()
dates_list <- list()

# Loop through each row of tabs
for (i in seq_along(tabs)) {
  row_content <- html_text(tabs[i])
  lines <- strsplit(row_content, "\n")[[1]]
  names <- grep("^\\d{4}$", lines)
  
  # Extract dates
  dates <- extract_dates(row_content)
  
  # Add tab name and dates to lists
  tab_names[i] <- paste("Tab", i+1)  # Assuming tabs start from Tab 1
  dates_list[[i]] <- dates
}

# Original full name for cities
cities_long <- rev(row.names(transposed.matrix))

# Make cities actually be the oad region (two/three letter)
# to help with downstream processing
cities <- c("arr", "bpa", "cc", "dfw", "elp", "hgb", "san", "netx", "vic")
date_vector_named <- vector(mode = "list", length = length(cities))
date_vector <- date_vector_named

for(i in 1:length(cities)){
  #print(i) # this is the date column (goes from 1-18)
  #print(floor(i/2+0.5)) # (this is the number of cities (goes from 1-9))
  city_name <- cities[i]
    
  first_half_idx <- 2*i - 1
  second_half_idx <- 2*i 
  
  first_half <- dates_list[[first_half_idx]]
  second_half <- dates_list[[second_half_idx]]
  combined_list <- c(first_half,second_half)
  combined_list_named <- c(city_name,first_half,second_half)
  
  date_vector[[i]] <- combined_list[substr(combined_list, 6, 8) != "/24"] # remove 2024
  date_vector[[i]] <- as.Date(date_vector[[i]],format = "%m/%d/%y")
}

### WRITING DATA TO FILES ####

for (i in 1:length(cities)){
  city <- cities[i]
  dates <- date_vector[[i]]
  fileName <- paste0(city,'_Action_Days.csv')
  write.csv(dates,paste0(prefix,fileName))
}