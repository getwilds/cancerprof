#' Access to Vaccines Data
#' 
#' This function returns a data frame from Vaccines in State Cancer Profiles
#'
#' @param vaccine Either "mammogram in past 2 years, ages 50-74", "mammogram in past 2 years, ages 40+", 
#'                       "pap smear in past 3 years, no hysterectomy, ages 21-65", 
#'                       "pap smear in past 3 years, no hysterectomy, ages 18+"
#' @param sex Either "both sexes", "males", "females"
#' 
#' @returns A data frame with the following columns "State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' risk_vaccine("percent with up to date hpv vaccination coverage, ages 13-15", "both sexes")
#' risk_vaccine("percent with up to date hpv vaccination coverage, ages 13-15", "both sexes")
#' risk_vaccine("percent with up to date hpv vaccination coverage, ages 13-15", "females")
#' }
risk_vaccine <- function(vaccine, sex) {
  
  req <- create_request("risk")
  
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
      setNames(c("State", "FIPS", "Met_Objective_of_80.0%?", "Percent", "Lower_95%_CI", "Upper_95%_CI", "Number_of_Respondents"))
  } else {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI", "Number_of_Respondents"))
  }
}
