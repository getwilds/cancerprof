#' Handles Vaccines Values to Code
#'
#' This function returns a matching code value for a Vaccines
#' for the api to use to get data from State Cancer Profiles
#'
#' @param vaccine One of the following values:
#' - `"percent with up to date hpv vaccination coverage, ages 13-15",`
#' - `"percent with up to date hpv vaccination coverage, ages 13-17"`.
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective Vaccine Value
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_vaccine("percent who received 2+ doses of HPV vaccine, ages 13-17")
#' }
handle_vaccine <- function(vaccine) {
  vaccine <- tolower(vaccine)

  vaccine_mapping <- c(
    "percent with up to date hpv vaccination coverage, ages 13-15" = "v281",
    "percent with up to date hpv vaccination coverage, ages 13-17" = "v282"
  )

  vaccine_code <- vaccine_mapping[vaccine]

  if (is_na(vaccine_code)) {
    stop(
      paste(
        "Invalid vaccine input, please check",
        "the documentation for valid inputs"
      )
    )
  }
  return(as.character(vaccine_code))
}
