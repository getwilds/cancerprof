#' Handles Trend Data Types to Code
#'
#' This function returns a matching code value for a Risk Data Types
#' for the api to use to get data from State Cancer Profiles
#'
#' @param datatype Either "incidence" or "mortality"
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective Risk Data Types Value
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_datatype("incidence")
#' }
handle_trend_datatype <- function(datatype) {
  datatype <- tolower(datatype)
  
  datatype_mapping <- c(
    "incidence" = "1",
    "mortality" = "2"
  )
  
  datatype_code <- datatype_mapping[datatype]
  
  if (is_na(datatype_code)) {
    stop(
      paste(
        "Invalid datatype input, please check",
        "the documentation for valid inputs"
      )
    )
  }
  
  return(as.character(datatype_code))
}
