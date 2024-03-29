#' Handles Cancer Incidence Year Values to Code
#'
#' This function returns a matching code value for a Cancer Incidence Year
#' for the api to use to get data from State Cancer Profiles
#'
#' @param year One of the following values:
#' - "latest 5 year average",
#' - "latest single year (us by state)"
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective Cancer Incidence Year
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_year("latest 5 year average")
#' }
handle_year <- function(year) {
  year <- tolower(year)

  year_mapping <- c(
    "latest 5 year average" = "0",
    "latest single year (us by state)" = "1"
  )

  year_code <- year_mapping[year]

  if (is_na(year_code)) {
    stop("Invalid year input, please check the documentation for valid inputs")
  }

  return(as.character(year_code))
}
