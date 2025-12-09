## Scraping Government data
## Website: GovInfo (https://www.govinfo.gov/app/search/)
## Prerequisite: Download from website the list of files to be downloaded
## Designed for background job

# Start with a clean plate and lean loading to save memory
 
gc(reset=T)

# install.packages(c("purrr", "magrittr")
library(purrr)
library(magrittr) # Alternatively, load tidyverse
library(rjson)
library(jsonlite)
library(data.table)
library(readr)

## CSV method (query url https://www.govinfo.gov/app/search/%7B%22query%22%3A%22%22%2C%22offset%22%3A0%2C%22pageSize%22%3A10%2C%22historical%22%3Afalse%2C%22facetToExpand%22%3A%22congressnum%22%2C%22facets%22%3A%7B%22companiesnav%22%3A%5B%22Committee%20on%20Foreign%20Affairs%22%5D%2C%22accodenav%22%3A%5B%22CHRG%22%5D%2C%22congressnum%22%3A%5B%22118%22%5D%2C%22governmentauthornav%22%3A%5B%22Congress%22%5D%7D%2C%22filterOrder%22%3A%5B%22companiesnav%22%2C%22accodenav%22%2C%22congressnum%22%2C%22governmentauthornav%22%5D%2C%22sortBy%22%3A%220%22%2C%22isLoading%22%3Afalse%7D)
govfiles= read.csv(file="data/118 doc list.csv")

# Preparing for bulk download of government documents
govfiles$id = govfiles$packageId
pdf_govfiles_url = govfiles$pdfLink
pdf_govfiles_id <- govfiles$index

# Directory to save the pdf's
# Be sure to create a folder for storing the pdf's
save_dir <- "data/govt/"

# Function to download pdfs
download_govfiles_pdf <- function(url, id) {
  tryCatch({
    destfile <- paste0(save_dir, "govfiles_", id, ".pdf")
    download.file(url, destfile = destfile, mode = "wb") # Binary files
    Sys.sleep(runif(1, 1, 3))  # Important: random sleep between 1 and 3 seconds to avoid suspicion of "hacking" the server
    return(paste("Successfully downloaded:", url))
  },
  error = function(e) {
    return(paste("Failed to download:", url))
  })
}

# Download files, potentially in parallel for speed
# Simple timer, can use package like tictoc
# 

## Try downloading one document
start.time <- Sys.time()
message("Starting downloads")
results <- 1:1 %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Try five
start.time <- Sys.time()
message("Starting downloads")
results <- 1:5 %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Print results
print(results)

## Download all: Caution, this may take a while and lots of space
start.time <- Sys.time()
message("Starting downloads")
results <- 1:length(pdf_govfiles_url) %>% 
  purrr::map_chr(~ download_govfiles_pdf(pdf_govfiles_url[.], pdf_govfiles_id[.]))
message("Finished downloads")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


## Exercise: Try downloading 118th Congress Congressional Hearings in Committee on Foreign Affairs?