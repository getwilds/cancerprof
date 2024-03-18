#' Handles workforce Values to Code
#'
#' This function returns a matching code value for workforce
#' for the api to use to get data from State Cancer Profiles
#'
#' @param workforce "unemployed"
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective workforce Value
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_workforce("unemployed")
#' }
handle_workforce <- function(workforce) {
  workforce <- tolower(workforce)

  workforce_mapping <- c(
    "unemployed" = "00012"
  )

  workforce_code <- workforce_mapping[workforce]

  if (is_na(workforce_code)) {
    stop(
      paste(
        "Invalid workforce input, please check",
        "the documentation for valid inputs"
      )
    )
  }

  return(as.character(workforce_code))
}
