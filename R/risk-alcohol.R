#' Access to Alcohol Screening and Risk Data
#' 
#' This function returns a data frame from Alcohol in State Cancer Profiles
#'
#' @param alcohol "binge drinking (4+ drinks on one occasion for women, 5+ drinks for one occasion for men), ages 21+"
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' 
#' @returns A data frame with the following columns "State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' risk_alcohol("binge drinking (4+ drinks on one occasion for women, 5+ drinks for one occasion for men), ages 21+", 
#'              "all races (includes hispanic)", "both sexes")
#' risk_alcohol("binge drinking (4+ drinks on one occasion for women, 5+ drinks for one occasion for men), ages 21+", 
#'              "hispanic (any race)", "females")
#' }
risk_alcohol <- function(alcohol, race, sex) {
  
  req <- create_request("risk")
  
  resp <- req %>%
    req_url_query(
      topic="alcohol",
      risk=handle_alcohol(alcohol),
      race=handle_race(race),
      sex=handle_sex(sex),
      type="risk",
      sortVariableName="percent",
      sortOrder="default",
      output=1
    ) %>%
    req_perform()
  
  resp <- process_screening(resp)
  
  resp %>% 
    setNames(c("State", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI", "Number_of_Respondents"))
}
