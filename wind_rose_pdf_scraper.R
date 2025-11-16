library(tidyverse)
library(pdftools)
library(stringr)

# Directory with all wind PDFs
pdf_dir <- "data/wind"

# List all PDF files
pdf_files <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

process_wind_pdf <- function(pdf_file) {
  pages <- pdf_text(pdf_file)
  doc_id <- basename(pdf_file)
  
  # --- 1. Extract coordinates (always on page 1) ---
  latlon <- str_match(
    pages[1],
    "Position lat/lon \\(WGS84\\):\\s*([0-9\\.\\-]+)\\s+([0-9\\.\\-]+)"
  )
  
  # If match failed, skip this file
  if (is.na(latlon[1])) {
    warning(paste("No coordinates found in", pdf_file))
    return(NULL)
  }
  
  lat <- as.numeric(latlon[2])
  lon <- as.numeric(latlon[3])
  
  # --- 2. Extract 10m / 0m roughness table (page 2) ---
  if (length(pages) < 2) {
    warning(paste("PDF", pdf_file, "does not have expected page 2"))
    return(NULL)
  }
  
  p2 <- pages[2]
  lines <- str_split(p2, "\n")[[1]] %>% str_squish()
  
  # Find Frequency row
  freq_line <- lines[str_detect(lines, "^10\\s+Frequency")]
  if (length(freq_line) == 0) {
    warning(paste("No frequency row in", pdf_file))
    return(NULL)
  }
  
  freq_vals <- str_extract_all(freq_line, "[0-9]+\\.?[0-9]*")[[1]] %>% as.numeric()
  
  # Extract first 12 values only (the 12 wind directions)
  freq_vals <- freq_vals[1:12]
  
  # Find mean windspeed row
  mean_line <- lines[str_detect(lines, "^10\\s+Mean windspeed")]
  if (length(mean_line) == 0) {
    warning(paste("No mean windspeed row in", pdf_file))
    return(NULL)
  }
  
  mean_vals <- str_extract_all(mean_line, "[0-9]+\\.?[0-9]*")[[1]] %>% as.numeric()
  mean_vals <- mean_vals[1:12]
  
  # --- 3. Determine dominant sector ---
  dirs <- seq(0, 330, by = 30)
  idx <- which.max(freq_vals)
  dom_dir <- dirs[idx]
  dom_ws <- mean_vals[idx]
  
  tibble(
    doc_id = doc_id,
    lat = lat,
    lon = lon,
    dominant_direction = dom_dir,
    dominant_windspeed = dom_ws
  )
}

# --- Process all PDFs and combine ---
wind_df <- map_dfr(pdf_files, process_wind_pdf)

# Show result
print(wind_df)
