#' Extract Values
#'
#' This function finds a string value that contains a key and removes the
#' key from the value
#'
#' @param key A String value
#' @param resp_metadaa A list of strings of metadata
#' 
#' @returns A string value without the key
#' 
#' @examples
#' \dontrun{
#' extract_values("Sorted by", resp_metadata)
#' }
extract_values <- function(key, resp_metadata) {
  values <- resp_metadata[grep(key, resp_metadata)]
  values <- gsub(paste0("^\\s*", key, ":?\\s*"), "", values)
  return(values)
}

#' Custom print function
#'
#' This custom print function processes the
#' metadata output for easier readability
#'
#' @param x
#'
#' @export
print.cancerprof_metadata <- function(x, ...) {

  cat("\033[38;5;246m# Data Report: \033[39m", "\n")
  cat(paste(x$data_report, '\n', sep = "", collapse = " "), "\n")
  
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
  cat("\n")
  
  if (!is.null(x$additional_notes) && length(x$additional_notes) > 0) {
    cat("\033[38;5;246m# Additional Notes: \033[39m", "\n")
    cat(x$additional_notes, "\n")
  }
  
  invisible(x)
}

#' Get Metadata
#'
#' This function assigns a list of metadata components and returns a string of
#' processed metadata that is easily readable
#'
#' @param input_tbl A tibble object
#' 
#' @returns a string of metadata and an invisible metadata object as a list 
#' of strings
#' 
#' @examples
#' \dontrun{
#' process_metadata(resp)
#' }
get_metadata <- function(input_tbl) {
  resp_metadata <- attr(input_tbl, "metadata")
  
  resp_metadata <- gsub("\\\"", "", resp_metadata)

  data_report <- c(resp_metadata[1], resp_metadata[2], resp_metadata[3], resp_metadata[4])
  sortedby <- extract_values("Sorted by", resp_metadata)
  createdby <- extract_values("Created by", resp_metadata)
  data_sources <- extract_values("Source", resp_metadata)
  data_dictionary <- resp_metadata[grep("For more information", resp_metadata)]
  data_limitations <- extract_values("Data for", resp_metadata)

  exclude_keywords <- c("Sorted by", "Created by", "Source", "For more information", "Data for")

  additional_notes <- resp_metadata[!grepl(paste(exclude_keywords, collapse = "|"), resp_metadata, ignore.case = TRUE)]
  
  additional_notes <- additional_notes[!additional_notes %in% data_report]
  
  demo_metadata_list <- list(
    data_report = data_report,
    sortedby = sortedby,
    createdby = createdby,
    data_sources = data_sources,
    data_dictionary = data_dictionary,
    data_limitations = data_limitations,
    additional_notes = additional_notes
  )
  
  class(demo_metadata_list) <- c("cancerprof_metadata", class(demo_metadata_list))
  
  print.cancerprof_metadata(demo_metadata_list)
}

get_raw_metadata <- function(input_tbl) {
  resp_metadata <- attr(input_tbl, "metadata")
  
  return(resp_metadata)
}
