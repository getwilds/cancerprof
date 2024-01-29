#' Access to Women's Health Data
#' 
#' This function returns a data frame from Women's Health in State Cancer Profiles
#'
#' @param whealth Either "mammogram in past 2 years, ages 50-74", "mammogram in past 2 years, ages 40+", 
#'                       "pap smear in past 3 years, no hysterectomy, ages 21-65", "pap smear in past 3 years, no hysterectomy, ages 18+"
#' @param race One of the following values: "all races (includes hispanic)", "white (non-hispanic)", 
#'                                          "black (non-hispanic)", "amer. indian / ak native (non-hispanic)", 
#'                                          "asian / pacific islander (non-hispanic)","hispanic (any race)"
#' @param datatype Either "direct estimates" or "county level modeled estimates"
#' @param area A state/territory abbreviation or USA.
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "People Unemployed", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' risk_whealth("mammogram in past 2 years, ages 50-74", 
#'              "all races (includes hispanic)", "direct estimates")
#' risk_whealth("pap smear in past 3 years, no hysterectomy, ages 21-65", 
#'              "all races (includes hispanic)", "county level modeled estimates", "wa")
#' risk_whealth("pap smear in past 3 years, no hysterectomy, ages 18+", 
#'              "all races (includes hispanic)", "county level modeled estimates", "wa")
#' risk_whealth("pap smear in past 3 years, no hysterectomy, ages 18+", "black (non-hispanic)")
#' }
risk_whealth <- function(whealth, race, datatype="direct estimates", area=NULL) {
  
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
      risk=handle_whealth(whealth),
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
    if(whealth == "pap smear in past 3 years, no hysterectomy, ages 18+") {
      resp %>% 
        setNames(c("State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"))
    } else {
      resp %>% 
        setNames(c("County", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI"))
    }
  } else {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"))
  }
}
