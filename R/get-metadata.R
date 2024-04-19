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
  
  data_topic <- attributes(x)$data_topic
  
  # do some conditionals to filter data topic
  if (data_topic == "demographics" || data_topic == "risks") {
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
    
    if (!is.null(x$data_sources) && length(x$data_sources) > 0) {
      cli_text("{.cancerprof_class # Data Sources:}", "\n")
      cli_text(x$data_sources, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$data_dictionary) && length(x$data_dictionary) > 0) {
      cli_text("{.cancerprof_class # Data Dictionary:}", "\n")
      cli_text(x$data_dictionary, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$data_limitations) && length(x$data_limitations) > 0) {
      cli_text("{.cancerprof_class # Data Limitations:}", "\n")
      cli_text(x$data_limitations, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$name_change) && length(x$name_change) > 0) {
      cli_text("{.cancerprof_class # Name Change:}", "\n")
      cli_text(x$name_change, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$additional_notes) && length(x$additional_notes) > 0) {
      cli_text("{.cancerprof_class # Additional Notes:}", "\n")
      cli_text(x$additional_notes, "\n")
    }
    
  } else if (data_topic == "incidence" || data_topic == "mortality") {
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
    
    if (!is.null(x$trend) && length(x$x$trend) > 0) {
      cli_text("{.cancerprof_class # Trend:}", "\n")
      cli_text(x$x$trend, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$trend_note) && length(x$trend_note) > 0) {
      cli_text("{.cancerprof_class # Trend Note:}", "\n")
      cli_text(x$trend_note, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$rate_note) && length(x$rate_note) > 0) {
      cli_text("{.cancerprof_class # Rate Note:}", "\n")
      cli_text(x$rate_note, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$stage_note) && length(x$stage_note) > 0) {
      cli_text("{.cancerprof_class # Stage Note:}", "\n")
      cli_text(x$stage_note, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$rank_note) && length(x$rank_note) > 0) {
      cli_text("{.cancerprof_class # Rank Note:}", "\n")
      cli_text(x$rank_note, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$data_not_available) && length(x$data_not_available) > 0) {
      cli_text("{.cancerprof_class # Data not available:}", "\n")
      cli_text(x$data_not_available, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$data_sources) && length(x$data_sources) > 0) {
      cli_text("{.cancerprof_class # Data sources:}", "\n")
      cli_text(x$data_sources, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$data_limitations) && length(x$data_limitations) > 0) {
      cli_text("{.cancerprof_class # Data limitations:}", "\n")
      cli_text(x$data_limitations, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$additional_notes) && length(x$additional_notes) > 0) {
      cli_text("{.cancerprof_class # Additional Notes:}", "\n")
      cli_text(x$additional_notes, "\n")
    }
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
  
  #check data topic
  data_topic <- attributes(input_tbl)$data_topic
  
  # do some conditionals to filter data topic
  if (data_topic == "demographics" || data_topic == "risks") {
    data_report <- c(resp_metadata[1], resp_metadata[2], resp_metadata[3], resp_metadata[4])
    sortedby <- extract_values("Sorted by", resp_metadata)
    createdby <- extract_values("Created by", resp_metadata)
    data_sources <- extract_values("Source", resp_metadata)
    data_dictionary <- resp_metadata[grep("For more information", resp_metadata)]
    data_limitations <- resp_metadata[grep("Data for", resp_metadata)]
    
    name_change <- extract_values("Name Change:", resp_metadata)
    
    exclude_keywords <- c("Sorted by", "Created by", "Source", "For more information", "Data for", "Name Change")
    
    additional_notes <- resp_metadata[!grepl(paste(exclude_keywords, collapse = "|"), resp_metadata, ignore.case = TRUE)]
    additional_notes <- additional_notes[!additional_notes %in% data_report]
    
    output_metadata_list <- list(
      data_report = data_report,
      sortedby = sortedby,
      createdby = createdby,
      data_sources = data_sources,
      data_dictionary = data_dictionary,
      data_limitations = data_limitations,
      additional_notes = additional_notes
    )
    
  } else if (data_topic == "incidence" || data_topic == "mortality") {
    data_report <- c(resp_metadata[1], resp_metadata[2], resp_metadata[3])
    sortedby <- extract_values("Sorted by", resp_metadata)
    createdby <- extract_values("Created by", resp_metadata)
    trend <- extract_values("^ ", resp_metadata)
    trend_note <- extract_values("trend note", resp_metadata)
    rate_note <- extract_values("rate note", resp_metadata)
    stage_note <- extract_values("Stage ", resp_metadata)
    rank_note <- extract_values("rank note", resp_metadata)
    data_not_available <- resp_metadata[grep("Data not available", resp_metadata)]
    data_sources <- extract_values("Source:", resp_metadata)
    data_limitations <- resp_metadata[grep("Data for", resp_metadata)]
    
    exclude_keywords <- c("Sorted by", "Created by", "^ ", "trend note", 
                          "rate note", "Stage ", "rank note", 
                          "Data not available",  "Source", "Data for")
    
    additional_notes <- resp_metadata[!grepl(paste(exclude_keywords, collapse = "|"), resp_metadata, ignore.case = TRUE)]
    additional_notes <- additional_notes[!additional_notes %in% data_report]
    
    output_metadata_list <- list(
      data_report = data_report,
      sortedby = sortedby,
      createdby = createdby,
      trend = trend,
      trend_note = trend_note,
      rate_note = rate_note,
      stage_note = stage_note,
      rank_note = rank_note,
      data_not_available = data_not_available,
      data_sources = data_sources,
      data_limitations = data_limitations,
      additional_notes = additional_notes
    )
    
  } else {
    cli_abort("Incorrect data topic argument, please ensure that it is correct.")
  }
  
  #add attribute to list
  attr(output_metadata_list, "data_topic") <- data_topic
  
  #add custom print
  class(output_metadata_list) <- c("cancerprof_metadata", class(output_metadata_list))
  
  print.cancerprof_metadata(output_metadata_list)
}

get_raw_metadata <- function(input_tbl) {
  resp_metadata <- attr(input_tbl, "metadata")
  
  return(resp_metadata)
}
