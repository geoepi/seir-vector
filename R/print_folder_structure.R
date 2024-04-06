print_folder_structure <- function(path, prefix = "") {
  contents <- list.files(path, full.names = TRUE)
  for (i in seq_along(contents)) {
    is_last <- i == length(contents)
    if (is_last) {
      cat(paste0(prefix, "└── ", basename(contents[i]), "\n"))
    } else {
      cat(paste0(prefix, "├── ", basename(contents[i]), "\n"))
    }
    
    if (is_last) {
      new_prefix <- paste0(prefix, "    ")
    } else {
      new_prefix <- paste0(prefix, "│   ")
    }
    
    if (file.info(contents[i])$isdir) {
      print_folder_structure(contents[i], new_prefix)
    }
  }
}