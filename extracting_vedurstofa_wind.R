library(httr)
library(stringr)

# Base URL pattern
url_pattern <- "https://vindatlas.vedur.is/en/pdf.php?pdf=true&id=%d&src=1&hl=1&dir=0"

# Folder to save PDFs
outdir <- "data/wind"
if (!dir.exists(outdir)) dir.create(outdir)

# Maximum ID
max_id <- 13000

# Function to safely download with retries
download_pdf <- function(id, retries = 3) {
  fname <- file.path(outdir, sprintf("vindatlas_id%05d.pdf", id))
  
  # Skip if file already exists
  if (file.exists(fname)) {
    message(sprintf("Skipping ID %d, already downloaded.", id))
    return(TRUE)
  }
  
  url <- sprintf(url_pattern, id)
  
  for (i in 1:retries) {
    try({
      res <- GET(url, 
                 user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/115.0 Safari/537.36"),
                 timeout(60))
      
      if (status_code(res) == 200) {
        writeBin(content(res, "raw"), fname)
        message(sprintf("Downloaded ID %d → %s", id, fname))
        return(TRUE)
      } else if (status_code(res) == 404) {
        message(sprintf("No file for ID %d (HTTP 404)", id))
        return(FALSE)
      } else {
        message(sprintf("HTTP %d for ID %d, retrying...", status_code(res), id))
      }
    }, silent = TRUE)
    
    Sys.sleep(2)  # polite retry delay
  }
  
  message(sprintf("Failed to download ID %d after %d retries", id, retries))
  return(FALSE)
}

# Loop over all IDs
for (id in 1:max_id) {
  download_pdf(id)
  Sys.sleep(1)  # polite delay
}
