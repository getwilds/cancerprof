#' Handles Crowding Values to Code
#'
#' This function returns a matching code value for Crowding
#' for the api to use to get data from State Cancer Profiles
#'
#' @param crowding "household with >1 person per room"
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective Crowding Value
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_crowding("household with >1 person per room")
#' }
handle_crowding <- function(crowding) {
  crowding <- tolower(crowding)

  crowding_mapping <- c(
    "household with >1 person per room" = "00027"
  )

  crowding_code <- crowding_mapping[crowding]

  if (is_na(crowding_code)) {
    stop(
      paste(
        "Invalid crowding input, please check",
        "the documentation for valid inputs"
      )
    )
  }

  return(as.character(crowding_code))
}
