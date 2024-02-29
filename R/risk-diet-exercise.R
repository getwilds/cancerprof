#' Access to Diet & Exercise Screening Data
#' 
#' This function returns a data frame from Diet & Exercise in State Cancer Profiles
#'
#' @param diet_exercise Either "bmi is healthy, ages 20+", "bmi is obese, ages 20+", 
#'                             "bmi is obese, high school survey", "bmi is overweight, high school survey",
#'                             "consumed 1 or more fruits per day", "consumed 1 or more vegetables per day",
#'                             "no leisure time physical activity"
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' 
#' @returns A data frame with the following columns: Area Type, Area Code, "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' risk_diet_exercise(diet_exercise = "bmi is healthy, ages 20+",
#'                    race = "all races (includes hispanic)", 
#'                    sex = "both sexes")
#' risk_diet_exercise(diet_exer"bmi is obese, high school survey", "all races (includes hispanic)", "males")
#' }
risk_diet_exercise <- function(diet_exercise, race, sex) {
  
  req <- create_request("risk")
  
  resp <- req %>%
    req_url_query(
      topic="dietex",
      risk=handle_diet_exercise(diet_exercise),
      race=handle_race(race),
      sex=handle_sex(sex),
      type="risk",
      sortVariableName="percent",
      sortOrder="default",
      output=1
    ) %>%
    req_perform()
  
  resp <- process_screening(resp)
  
  diet_exercise_type1 = c("bmi is overweight, high school survey",
                          "bmi is obese, high school survey")
  
  if (diet_exercise %in% diet_exercise_type1) {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI"))
  } else {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI", "Number_of_Respondents"))
  }
}
