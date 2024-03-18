#' Access to Diet & Exercise Screening Data
#'
#' This function returns a data frame about diet and exercise risk
#' from State Cancer Profiles.
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
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Percent,
#' Lower 95% CI, Upper 95% CI, Number of Respondents.
#'
#' @family risks
#'
#' @export
#'
#' @examples
#' \dontrun{
#' risk_diet_exercise(
#'   diet_exercise = "bmi is healthy, ages 20+",
#'   race = "all races (includes hispanic)",
#'   sex = "both sexes"
#' )
#' risk_diet_exercise(
#'   diet_exercise = "bmi is obese, high school survey",
#'   race = "all races (includes hispanic)",
#'   sex = "males"
#' )
#' }
risk_diet_exercise <- function(diet_exercise, race, sex) {
  req <- create_request("risk")

  resp <- req %>%
    req_url_query(
      topic = "dietex",
      risk = handle_diet_exercise(diet_exercise),
      race = handle_race(race),
      sex = handle_sex(sex),
      type = "risk",
      sortVariableName = "percent",
      sortOrder = "default",
      output = 1
    ) %>%
    req_perform()

  resp <- process_resp(resp, "risks")

  diet_exercise_type1 <- c(
    "bmi is overweight, high school survey",
    "bmi is obese, high school survey"
  )

  if (diet_exercise %in% diet_exercise_type1) {
    resp %>%
      setNames(c(
        "State",
        "FIPS",
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI"
      )) %>%
      mutate(across(c(
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI"
      ), \(x) as.numeric(x)))
  } else {
    resp %>%
      setNames(c(
        "State",
        "FIPS",
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI",
        "Number_of_Respondents"
      )) %>%
      mutate(across(c(
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI",
        "Number_of_Respondents"
      ), \(x) as.numeric(x)))
  }
}
