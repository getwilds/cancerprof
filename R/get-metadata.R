
#helper function to remove key from values
extract_values <- function(key, resp_metadata) {
  values <- resp_metadata[grep(key, resp_metadata)]
  values <- gsub(paste0("^\\s*", key, ":?\\s*"), "", values)
  return(values)
}

#' Custom print function
#'
#' This custom print function processes the
#' metadata output for a response object
#'
#' @param x
#'
#' @export
print.cancerprof_metadata <- function(x, ...) {

  cat("\033[38;5;246m# Data Report: \033[39m", "\n")
  cat(paste(x$data_report, '"\n', sep = "", collapse = " "), "\n")
  
  cat("\033[38;5;246m# Sorted By: \033[39m", "\n")
  cat(x$sortedby, "\n")
  cat("\n")
  
  cat("\033[38;5;246m# Created By: \033[39m", "\n")
  cat(x$createdby, "\n")
  cat("\n")
  
  cat("\033[38;5;246m# Data Sources: \033[39m", "\n")
  cat(x$data_sources, "\n")
  cat("\n")
  
  cat("\033[38;5;246m# Data Dictionary: \033[39m", "\n")
  cat(x$data_dictionary, "\n")
  cat("\n")
  
  cat("\033[38;5;246m# Data Limitations: \033[39m", "\n")
  cat(x$data_limitations, "\n")
  
  invisible(x)
}


get_metadata <- function(input_tbl) {
  resp_metadata <- attr(input_tbl, "metadata")
  
  resp_metadata <- gsub("\\\"", "", resp_metadata)

  data_report <- c(resp_metadata[1], resp_metadata[2], resp_metadata[3], resp_metadata[4])
  sortedby <- extract_values("Sorted by", resp_metadata)
  createdby <- extract_values("Created by", resp_metadata)
  data_sources <- extract_values("Source", resp_metadata)
  data_dictionary <- extract_values("For more information about", resp_metadata)
  data_limitations <- extract_values("Data for", resp_metadata)


  demo_metadata_list <- list(
    data_report = data_report,
    sortedby = sortedby,
    createdby = createdby,
    data_sources = data_sources,
    data_dictionary = data_dictionary,
    data_limitations = data_limitations
  )
  
  class(demo_metadata_list) <- c("cancerprof_metadata", class(demo_metadata_list))
  
  print.cancerprof_metadata(demo_metadata_list)
}
