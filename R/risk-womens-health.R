#' Access to Women's Health Data
#' 
#' This function returns a data frame from Women's Health in State Cancer Profiles
#'
#' @param women_health Either "mammogram in past 2 years, ages 50-74", "mammogram in past 2 years, ages 40+", 
#'                       "pap smear in past 3 years, no hysterectomy, ages 21-65"
#' @param race One of the following values: "all races (includes hispanic)", "white (non-hispanic)", 
#'                                          "black (non-hispanic)", "amer. indian / ak native (non-hispanic)", 
#'                                          "asian / pacific islander (non-hispanic)","hispanic (any race)"
#' @param datatype Either "direct estimates" or "county level modeled estimates"
#' @param area A state/territory abbreviation or USA.
#' 
#' @returns A data frame with the following columns: Area Type, Area Code, "Percent", "People Unemployed", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' risk_women_health(women_health = "mammogram in past 2 years, ages 50-74", 
#'                   race = "all races (includes hispanic)",
#'                   datatype = "direct estimates")
#'                   
#' risk_women_health(women_health = "pap smear in past 3 years, no hysterectomy, ages 21-65", 
#'                   race = "all races (includes hispanic)",
#'                   datatype = "county level modeled estimates",
#'                   area = "wa")
#'                   
#' risk_women_health(women_health = "pap smear in past 3 years, no hysteroetomy, ages 21-65",
#'                   race = "black (non-hispanic)")
#' }
risk_women_health <- function(women_health, race, datatype="direct estimates", area=NULL) {
  
  req <- create_request("risk")
  
  risk_races = c("all races (includes hispanic)", "white (non-hispanic)", "black (non-hispanic)",
                 "amer. indian / ak native (non-hispanic)", "asian / pacific islander (non-hispanic)",
                 "hispanic (any race)")
  
  if (race == "all races (includes hispanic)" && is.null(datatype)) {
    cli_abort("For all races (includes hispanic), datatype must NOT be NULL")
  } else if ((race %in% risk_races && race != "all races (includes hispanic)") && (!is.null(area))) {
    cli_abort("For races other than all races (includes hispanic), area must be NULL")
  } else if ((race == "all races (includes hispanic)" && datatype == "county level modeled estimates") && is.null(area)) {
    cli_abort("For county level modeled estimates, Area must NOT be NULL")
  }

  resp <- req %>%
    req_url_query(
      topic="women",
      risk=handle_women_health(women_health),
      race=handle_race(race),
      type="risk",
      sortVariableName="percent",
      sortOrder="default",
      output=1
    )
  
  if(!is.null(datatype)) {
    resp <- resp %>%
      req_url_query(datatype=handle_datatype(datatype))
  }
  
  if(!is.null(area)) {
    resp <- resp %>%
      req_url_query(stateFIPS=fips_scp(area))
  }
  
  resp <- resp %>%
    req_perform()
  
  resp <- process_screening(resp)
  
  if (datatype == "county level modeled estimates") {
    if(women_health == "pap smear in past 3 years, no hysterectomy, ages 21-65") {
      resp %>% 
        setNames(c("State", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI", "Number_of_Respondents"))
    } else {
      resp %>% 
        setNames(c("County", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI"))
    }
  } else {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower_95%_CI", "Upper_95%_CI", "Number_of_Respondents"))
  }
}
