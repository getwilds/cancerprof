#' Handles Diet & Exercise Values to Code
#'
#' This function returns a matching code value for a Diet & Exercise
#' for the api to use to get data from State Cancer Profiles
#'
#' @param diet_exercise One of the following values:
#' - `"bmi is healthy, ages 20+"`
#' - `"bmi is obese, ages 20+"`
#' - `"bmi is obese, high school survey"`
#' - `"bmi is overweight, high school survey"`
#' - `"consumed 1 or more fruits per day"`
#' - `"consumed 1 or more vegetables per day"`
#' - `"no leisure time physical activity"`.
#' @param race One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White (non-Hispanic)"`
#' - `"Black (non-Hispanic)"`
#' - `"American Indian / Alaska Native (non-Hispanic)"`
#' - `"Asian / Pacific Islander (non-Hispanic)"`
#' - `"Hispanic (Any Race)"`.
#' @param sex One of the following values:
#' - `"both sexes"`
#' - `"male"`
#' - `"female"`.
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective Diet & Exercise Value
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_diet_exercise("guidance sufficient crc, ages 50-75")
#' }
handle_diet_exercise <- function(diet_exercise) {
  diet_exercise <- tolower(diet_exercise)

  diet_exercise_mapping <- c(
    "bmi is healthy, ages 20+" = "v01",
    "bmi is obese, ages 20+" = "v02",
    "bmi is obese, high school survey" = "v82",
    "bmi is overweight, high school survey" = "v83",
    "consumed 1 or more fruits per day" = "v50",
    "consumed 1 or more vegetables per day" = "v51",
    "no leisure time physical activity" = "v18"
  )

  diet_exercise_code <- diet_exercise_mapping[diet_exercise]

  if (is_na(diet_exercise_code)) {
    stop(
      paste(
        "Invalid diet-exercise input, please check",
        "the documentation for valid inputs"
      )
    )
  }

  return(as.character(diet_exercise_code))
}
