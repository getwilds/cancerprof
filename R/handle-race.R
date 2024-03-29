#' Handles Race Values to Code
#'
#' This function returns a matching code value for a race
#' for the api to use to get data from State Cancer Profiles
#'
#' @param race One of the following values:
#' - "all races (includes hispanic)"
#' - "white (includes hispanic)"
#' - "white non-hispanic"
#' - "black"
#' - "black (includes hispanic)"
#' - "amer. indian/alaskan native (includes hispanic)"
#' - "american indian/alaska native"
#' - "asian or pacific islander (includes hispanic)"
#' - "asian/pacific islander"
#' - "hispanic (any race)"
#' - "white (non-hispanic)"
#' - "black (non-hispanic)"
#' - "amer. indian / ak native (non-hispanic)"
#' - "american indian / alaska native (non-hispanic)"
#' - "asian / pacific islander (non-hispanic)"
#' - "asian (non-hispanic)"
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective race
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_race("all races (includes hispanic)")
#' handle_race("black (non-hispanic)")
#' }
handle_race <- function(race) {
  race <- tolower(race)

  race_mapping <- c(
    # demographic
    "all races (includes hispanic)" = "00",
    "white (includes hispanic)" = "01",
    "white non-hispanic" = "07",
    "black" = "02",
    "black (includes hispanic)" = "02",
    "amer. indian/alaskan native (includes hispanic)" = "03",
    "american indian/alaska native" = "03",
    "asian or pacific islander (includes hispanic)" = "04",
    "asian/pacific islander" = "04",
    "hispanic (any race)" = "05",

    # screening and risk factors
    "white (non-hispanic)" = "07",
    "black (non-hispanic)" = "28",
    "amer. indian / ak native (non-hispanic)" = "38",
    "american indian / alaska native (non-hispanic)" = "38",
    "asian / pacific islander (non-hispanic)" = "48",
    "asian (non-hispanic)" = "49"
  )

  code <- race_mapping[race]

  if (is_na(code)) {
    stop("Invalid race input, please check the documentation for valid inputs")
  }

  return(as.character(code))
}
