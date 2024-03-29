#' Handles Poverty Values to Code
#'
#' This function returns a matching code value for a Poverty
#' for the api to use to get data from State Cancer Profiles
#'
#' @param poverty One of the following values:
#' - `"families below poverty"`
#' - `"persistent poverty"`
#' - `"persons below poverty"`
#' - `"persons < 150% of poverty"`.
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective poverty variable
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_poverty("families below poverty")
#' }
handle_poverty <- function(poverty) {
  poverty <- tolower(poverty)

  poverty_mapping <- c(
    "families below poverty" = "00007",
    "persistent poverty" = "03001",
    "persons below poverty" = "00008",
    "persons < 150% of poverty" = "00009"
  )

  poverty_code <- poverty_mapping[poverty]

  if (is_na(poverty_code)) {
    stop(
      paste(
        "Invalid poverty input, please check",
        "the documentation for valid inputs"
      )
    )
  }

  return(as.character(poverty_code))
}
