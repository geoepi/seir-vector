get_data_osf <- function(get_data, destfile = NULL) {

  valid_data <- c("prior_model", "fit_2014", "fit_2015", "vs_inc_truth")
  if (!get_data %in% valid_data) {
    stop("Invalid model selected. Choose one of: 'vs_inc_truth', 'prior_model', 'fit_2014', 'fit_2015'")
  }

  url <- switch(
    get_data,
    "prior_model" = "https://osf.io/download/rjufz/",
    "fit_2014" = "https://osf.io/download/dwxpg/",
    "fit_2015" = "https://osf.io/download/dpj9x/",
    "vs_inc_truth" = "https://osf.io/download/va8hd/"
  )

  if (is.null(destfile)) {
    destfile <- paste0(get_data, ".rds")
  }
  
  if (!require("httr")) install.packages("httr", dependencies = TRUE)
  library(httr)
  
  response <- GET(url, write_disk(destfile, overwrite = TRUE))
  
  if (response$status_code == 200) {
    cat("File downloaded successfully and saved as", destfile, "\n")
    
    data <- readRDS(destfile)
    return(data)
  } else {
    stop("Failed to download the file. Status code:", response$status_code)
  }
}
