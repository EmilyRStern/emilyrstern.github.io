
library(govinfoR)
library(dplyr)
library(pdftools)
library(tm)


set_govinfo_key(key = #API KEY) 
                
# Get documents
docs <- gpo_collections(collection = "CRPT AND foreign relations committee", 
                        start_date = "2024-02-17T00:00:00Z") %>%
  arrange(date_issued)

recent <- docs %>% slice_tail(n = 100)

# Set your save directory
save_dir <- "data/govt"
dir.create(save_dir, showWarnings = FALSE)

download_govfiles_pdf <- function(url, id) {
  tryCatch({
    destfile <- paste0(save_dir, "govfiles_", id, ".pdf")
    
    # Check if URL exists first
    response <- httr::HEAD(url)
    if (httr::http_error(response)) {
      message("PDF not available: ", id)
      return(NULL)
    }
    
    download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
    Sys.sleep(runif(1, 1, 3))
    
    text <- pdftools::pdf_text(destfile)
    message("Success: ", id)
    return(paste(text, collapse = "\n"))
  },
  error = function(e) {
    message("Failed: ", id, " - ", e$message)
    return(NULL)
  })
}

create_corpus <- function(urls, ids) {
  texts <- mapply(download_govfiles_pdf, urls, ids, SIMPLIFY = FALSE)
  texts <- texts[!sapply(texts, is.null)]
  tm::Corpus(tm::VectorSource(texts))
}

pdf_urls <- paste0("https://www.govinfo.gov/content/pkg/", 
                   recent$package_id, "/pdf/", recent$package_id, ".pdf")
ids <- recent$package_id 

corpus <- create_corpus(urls = pdf_urls, ids = ids)

