#' Access to Colorectal Screening Data
#' 
#' This function returns a data frame from Colorectal Screening in State Cancer Profiles
#'
#' @param screening One of the following values: "ever had colorectal endoscopy (sigmoidoscopy or colonoscopy), ages 50+", 
#'                                                "fobt (1yr) / flex sig (5yr) / fobt (3yr) / colonoscopy (10yr), ages 50-75", 
#'                                                "home-based fecal occult blood test (fobt) in past two years", 
#'                                                "home-based fobt in the past two years or ever had a colorectal endoscopy", "ever had fobt, ages 50-75", 
#'                                                "guidance sufficient crc, ages 50-75", "had colonoscopy in past 10 years, ages 50-75"
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' @param area A state/territory abbreviation or USA.
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "People Unemployed", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' risk_colorectal_screening("ever had colorectal endoscopy (sigmoidoscopy or colonoscopy), ages 50+",
#'                           "all races (includes hispanic)","both sexes")
#' risk_colorectal_screening("ever had fobt, ages 50-75", area="usa")
#' risk_colorectal_screening("ever had fobt, ages 50-75", area="wa")
#' risk_colorectal_screening("fobt (1yr) / flex sig (5yr) / fobt (3yr) / colonoscopy (10yr), ages 50-75", 
#'                           "all races (includes hispanic)", "males" )

#' }
risk_colorectal_screening <- function(screening, race=NULL, sex=NULL, area=NULL) {
  
  req <- create_request("risk")
  
  screening_type_1 = c("ever had colorectal endoscopy (sigmoidoscopy or colonoscopy), ages 50+",
                       "fobt (1yr) / flex sig (5yr) / fobt (3yr) / colonoscopy (10yr), ages 50-75",
                       "home-based fecal occult blood test (fobt) in past two years",
                       "home-based fobt in the past two years or ever had a colorectal endoscopy")
  
  screening_type_2 = c("ever had fobt, ages 50-75",
                       "guidance sufficient crc, ages 50-75",
                       "had colonoscopy in past 10 years, ages 50-75")
  
  if (screening %in% screening_type_1 && (is.null(race) || is.null(sex))) {
    cli_abort("For this screening type, Race and Sex must not be NULL")
  } else if (screening %in% screening_type_2 && (is.null(area) || (!is.null(race) || !is.null(sex)))) {
    cli_abort("for this screening type, area must NOT be NULL and Race and Sex must be NULL")
  }

  resp <- req %>%
    req_url_query(
      topic="colorec",
      risk=handle_screening(screening),
      type="risk",
      sortVariableName="percent",
      sortOrder="default",
      output=1
    )
  
  if(!is.null(race)) {
    resp <- resp %>%
      req_url_query(race=handle_race(race))
  }
  
  if(!is.null(sex)) {
    resp <- resp %>%
      req_url_query(sex=handle_sex(sex))
  }
  
  if(!is.null(area)) {
    resp <- resp %>%
      req_url_query(stateFIPS=fips_scp(area))
  }
  
  resp <- resp %>%
    req_perform()
  
  resp <- process_screening(resp)

  if (screening %in% screening_type_1) {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI", "Number_of_Respondents"))
  } else if (screening %in% screening_type_2) {
    resp %>% 
      setNames(c("County", "FIPS", "Model_Based_Percent (95%_Confidence_Interval)", "Lower_95%_CI", "Upper_95%_CI"))
  }
}
