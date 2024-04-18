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
  
  cli_h1("Metadata")
  cli_text("\n")
  
  cli_div(theme = list(
    span.cancerprof_class = list(color = "darkgray")))
  
  cli_text("{.cancerprof_class # Data Report:}")
  for (i in seq_along(x$data_report)) {
    cli_text(x$data_report[i], "\n")
  }
  cli_text("\n")
  
  cli_text("{.cancerprof_class # Sorted By:}")
  cli_text(x$sortedby, "\n")
  cli_text("\n")
  
  cli_text("{.cancerprof_class # Created By:}")
  cli_text(x$createdby, "\n")
  cli_text("\n")
  
  cli_text("{.cancerprof_class # Data Sources:}")
  cli_text(x$data_sources, "\n")
  cli_text("\n")
  
  cli_text("{.cancerprof_class # Data Dictionary:}")
  cli_text(x$data_dictionary, "\n")
  cli_text("\n")
  
  cli_text("{.cancerprof_class # Data Limitations:}")
  cli_text(x$data_limitations, "\n")
  cli_text("\n")
  
  if (!is.null(x$additional_notes) && length(x$additional_notes) > 0) {
    cli_text("{.cancerprof_class # Additional Notes:}", "\n")
    cli_text(x$additional_notes, "\n")
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
  
  #check data topic somehow
  data_topic <- attributes(input_tbl)$data_topic
  
  # do some conditionals to filter data topic
  if (data_topic == "demographics") {
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
  } else if (data_topic == "risks") {
    
  } else if (data_topic == "incidence") {
    data_report <- c(resp_metadata[1], resp_metadata[2], resp_metadata[3])
    sortedby <- extract_values("Sorted by", resp_metadata)
    createdby <- extract_values("Created by", resp_metadata)
    trend <- extract_values("Trend", resp_metadata)
    rate_note <- extract_values("rate note", resp_metadata)
    stage_note <- extract_values("^ ", resp_metadata)
    rank_note <- extract_values("rank note", resp_metadata)
    data_not_available <- extract_values("Data not available", resp_metadata)
    
    
  } else if (data_topic == "mortality") {
    data_report <- c(resp_metadata[1], resp_metadata[2], resp_metadata[3])
    sortedby <- extract_values("Sorted by", resp_metadata)
    createdby <- extract_values("Created by", resp_metadata)
  } else {
    cli_abort("Incorrect data topic argument, please ensure that it is correct.")
  }
  

  
  
  class(demo_metadata_list) <- c("cancerprof_metadata", class(demo_metadata_list))
  
  print.cancerprof_metadata(demo_metadata_list)
}

get_raw_metadata <- function(input_tbl) {
  resp_metadata <- attr(input_tbl, "metadata")
  
  return(resp_metadata)
}
