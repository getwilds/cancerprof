#' Handles population Values to Code
#'
#' This function returns a matching code value for a population
#' for the api to use to get data from State Cancer Profiles
#'
#' @param population One of the following values:
#' - `"age under 18"`
#' - `"age 18-39"`
#' - `"age 40-64"`
#' - `"ages 40 and over"`
#' - `"ages 50 and over"`
#' - `"ages 60 and over"`
#' - `"american indian/alaska native"`
#' - `"asian/pacific islander"`
#' - `"black"`
#' - `"foreign born"`
#' - `"hispanic"`
#' - `"non-hispanic (origin recode)"`
#' - `"white"`
#' - `"males"`
#' - `"females"`.
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective population
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_population("males")
#' }
handle_population <- function(population) {
  population <- tolower(population)

  population_mapping <- c(
    "age under 18" = "00002",
    "age 18-39" = "00102",
    "age 40-64" = "00103",
    "ages 40 and over" = "00028",
    "ages 50 and over" = "00029",
    "ages 60 and over" = "00003",
    "american indian/alaska native" = "00023",
    "asian/pacific islander" = "00024",
    "black" = "00022",
    "foreign born" = "00014",
    "hispanic" = "00026",
    "non-hispanic (origin recode)" = "00101",
    "white" = "00025",
    "males" = "00104",
    "females" = "00105"
  )

  population_code <- population_mapping[population]

  if (is_na(population_code)) {
    stop(
      paste(
        "Invalid population input, please check",
        "the documentation for valid inputs"
      )
    )
  }

  return(as.character(population_code))
}
