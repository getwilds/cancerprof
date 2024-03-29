#' Handles Risk Data Types to Code
#'
#' This function returns a matching code value for a Risk Data Types
#' for the api to use to get data from State Cancer Profiles
#'
#' @param datatype Either "direct estimates" or "county level modeled estimates"
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective Risk Data Types Value
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_datatype("direct estimates")
#' }
handle_datatype <- function(datatype) {
  datatype <- tolower(datatype)

  datatype_mapping <- c(
    "direct estimates" = "0",
    "county level modeled estimates" = "1"
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
