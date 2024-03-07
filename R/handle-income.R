#' Handles Income Values to Code
#'
#' This function returns a matching code value for a Income
#' for the api to use to get data from State Cancer Profiles
#'
#' @param income Either "median family income" or "median household income"
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective income
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_income("median family income")
#' }
handle_income <- function(income) {
  income <- tolower(income)

  income_mapping <- c(
    "median family income" = "00010",
    "median household income" = "00011"
  )

  income_code <- income_mapping[income]

  if (is_na(income_code)) {
    stop(
      paste(
        "Invalid income input, please check",
        "the documentation for valid inputs"
      )
    )
  }

  return(as.character(income_code))
}
