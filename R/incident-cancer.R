#' Access to Cancer Incident Data
#' 
#' This function returns a data frame from Cancer Incident in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param cancer description
#' @param race One of the following values: "all races (includes hispanic)", "white (non-hispanic)", 
#'                                          "black (non-hispanic)", "amer. indian / ak native (non-hispanic)", 
#'                                          "asian / pacific islander (non-hispanic)","hispanic (any race)"
#' @param sex Either "both sexes", "males", "females"
#' @param age Either "all ages", "ages <50", "ages 50+", "ages <65", "ages 65+"
#' @param stage Either "all stages" or "late stage (regional & distant)"
#' 
#' @returns A data frame with the following columns "State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' incidence_cancer("percent who received 2+ doses of HPV vaccine, ages 13-15", "both sexes")
#' incidence_cancer("percent who received 2+ doses of HPV vaccine, ages 13-17", "both sexes")
#' incidence_cancer("percent who received 3+ doses of HPV vaccine, ages 13-17", "females")
#' }
incidence_cancer <- function(area, areatype, cancer, race, sex, age, stage, year) {
  
  req <- create_request("incidencerates")
  
  resp <- req %>%
    req_url_query(
      topic="vaccine",
      risk=handle_vaccine(vaccine),
      sex=handle_sex(sex),
      type="risk",
      sortVariableName="percent",
      sortOrder="default",
      output=1
    )
  
  resp <- resp %>%
    req_perform()
  
  resp <- process_screening(resp)
  
  
  vaccine_type1 = c("percent who received 2+ doses of HPV vaccine, ages 13-15",
                    "percent who received 3+ doses of HPV vaccine, ages 13-15"
  )
  
  vaccine_type2 = c("percent who received 2+ doses of HPV vaccine, ages 13-17",
                    "percent who received 3+ doses of HPV vaccine, ages 13-17"
  )
  
  if (vaccine %in% vaccine_type1) {
    resp %>% 
      setNames(c("State", "FIPS", "Met Objective of 80.0%?", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"))
  } else {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"))
  }
}
