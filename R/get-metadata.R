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
    
  } else if (data_topic == "trend") {
    data_report <- c(resp_metadata[1], resp_metadata[2])
    
    #get index for recent trend lines
    start_index <- grep("Section 1", resp_metadata)
    end_index <- grep("Notes:", resp_metadata) - 1
    
    recent_trend <- resp_metadata[start_index:end_index]
    createdby <- extract_values("Created by", resp_metadata)
    regression_note <- resp_metadata[grep("Regression lines", resp_metadata)]
    data_sources <- extract_values("Source:", resp_metadata)
    
    exclude_keywords <- c("Created by", "Section 1", "Notes:", "Regression lines", "Source")
    additional_notes <- resp_metadata[!grepl(paste(exclude_keywords, collapse = "|"), resp_metadata, ignore.case = TRUE)]
    additional_notes <- additional_notes[!additional_notes %in% c(data_report, recent_trend)]
    
    output_metadata_list <- list(
      data_report = data_report,
      recent_trend = recent_trend,
      createdby = createdby,
      regression_note = regression_note,
      data_sources = data_sources,
      additional_notes = additional_notes
    )
    
  } else {
    cli_abort("Incorrect data topic argument, please ensure that it is correct.")
  }
  
  #add attribute to list
  attr(output_metadata_list, "data_topic") <- data_topic
  
  class(output_metadata_list) <- c("cancerprof_metadata", class(output_metadata_list))
  
  output_metadata_list
}

get_raw_metadata <- function(input_tbl) {
  resp_metadata <- attr(input_tbl, "metadata")
  
  return(resp_metadata)
}
