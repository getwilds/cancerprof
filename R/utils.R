#' Get area titles and codes based on area type
#'
#' This function returns the title and code corresponding to the specified area type.
#'
#' @param areatype One of the following values:
#' - "county"
#' - "hsa"
#' - "state".
#' @return A character vector containing the title and code of the specified area type.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_area("county")
#' get_area("hsa")
#' }
get_area <- function(areatype) {
  areatype_map <- c("county" = "County", "hsa" = "Health_Service_Area", "state" = "State")
  areacode_map <- c("county" = "FIPS", "state" = "FIPS", "hsa" = "HSA_Code")

  areatype_title <- areatype_map[areatype]
  areacode_title <- areacode_map[areatype]

  return(c(areatype_title, areacode_title))
}

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
#' metadata output for each data topic for better readability
#'
#' @param x
#'
#' @export
print.cancerprof_metadata <- function(x, pretty_print = TRUE, ...) {
  
  if (!pretty_print) {
    class(x) <- class(x)[!(class(x) == "cancerprof_metadata")]
    return(x)
  }
  
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

  } else if (data_topic == "trend") {
    cli_text("{.cancerprof_class # Data Report:}")
    
    for (i in seq_along(x$data_report)) {
      cli_text(x$data_report[i], "\n")
    }
    cli_text("\n")
    
    cli_text("{.cancerprof_class # Recent Trend:}")
    cli_text(x$recent_trend, "\n")
    cli_text("\n")
    
    cli_text("{.cancerprof_class # Created By:}")
    cli_text(x$createdby, "\n")
    cli_text("\n")
    
    if (!is.null(x$regression_note) && length(x$regression_note) > 0) {
      cli_text("{.cancerprof_class # Regression Note:}", "\n")
      cli_text(x$regression_note, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$data_sources) && length(x$data_sources) > 0) {
      cli_text("{.cancerprof_class # Data Sources:}", "\n")
      cli_text(x$data_sources, "\n")
      cli_text("\n")
    }
    
    if (!is.null(x$additional_notes) && length(x$additional_notes) > 0) {
      cli_text("{.cancerprof_class # Additional Notes:}", "\n")
      cli_text(x$additional_notes, "\n")
    }
  }
  
  invisible(x)
}


#' Custom print function
#'
#' This custom print function edits the comment on the
#' metadata tibble output for a response object
#'
#' @param x
#'
#' @export
print.cancerprof_tbl <- function(x, ...) {
  
  primary_data_topics <- c("demographics, risks", "incidence", "mortality")
  data_topic <- attributes(x)$data_topic
  original_url <- attributes(x)$url
  
  if (data_topic == "trend") {
    modified_url <- gsub("data.php/historicaltrend.csv/", "index.php", original_url)

  } else if (data_topic %in% primary_data_topics) {
    modified_url <- gsub("&output=1", "#results", original_url)
    
  } else {
    cli_abort("Incorrect data topic, please ensure data topic is correct")
  }
  
  if (length(rownames(x)) == 0) {
    cli_par()
    cli_text(
      make_ansi_style("darkgrey")("{.cancerprof_class # No data was returned for this selection because the data is supressed due to insufficient counts}")
    )
    cli_text(
      make_ansi_style("darkgrey")("{.href [# Click to view this query on State Cancer Profiles](", modified_url, ")}")
    )
  } else if (length(rownames(x)) > 0) {
    cli_par()
    cli_text(
      make_ansi_style("darkgrey")("{.href [# Click to view this query on State Cancer Profiles](", modified_url, ")}")
    )
    cli_text(
      make_ansi_style("darkgrey")("{.cancerprof_class # Access metadata with `get_metadata()`}")
    )
  }

  NextMethod(x, ...)
}
