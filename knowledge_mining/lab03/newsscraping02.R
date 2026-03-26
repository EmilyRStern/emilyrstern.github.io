### Workshop: Scraping Google News RSS

# Check and install required packages if not already installed
packageneeded = c("tidyverse", "tidyRSS", "dplyr","tidyRSS")
if (!requireNamespace("packageneeded", quietly = TRUE))
  install.packages(packageneeded, dependencies = TRUE)
lapply(packageneeded, require, character.only = TRUE) # load packages

# Function to scrape RSS by query and time
scrape_google_news_rss_dates <- function(keyword, start_date, end_date) {
  
  # Create search string
  query <- paste0(keyword, " after:", start_date, " before:", end_date)
  encoded_query <- URLencode(query)
  
  # Form RSS URL
  rss_url <- paste0("https://news.google.com/rss/search?q=", encoded_query, "&hl=en-US&gl=US&ceid=US:en")
  
  # Fetch RSS feed
  rss_feed <- tidyRSS::tidyfeed(rss_url)
  
  # Extract data
  news_data <- rss_feed %>%
    select(item_title, item_description, item_link, item_pub_date)
  
  return(news_data)
}

# Example: Fetch Tariff news between January and March 2025
iranwar_2026 <- scrape_google_news_rss_dates("Iran war", "2026-02-28", "2026-03-23")


keywords <- c("Trump", "Israel", "Iran","Tehran")

iw_news <- bind_rows(
  lapply(keywords, function(k) scrape_google_news_rss_dates(k, "2026-02-28", "2026-03-23"))
)

# Remove duplicate titles or links
iw_news1 <- distinct(iw_news, item_title, .keep_all = TRUE)

# View
print(nrow(iw_news1))


