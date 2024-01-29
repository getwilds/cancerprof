#' Access to Smoking Data
#' 
#' This function returns a data frame from Smoking in State Cancer Profiles
#'
#' @param smoking Either "smoking laws (any)", "smoking laws (bars)", "smoking laws (restaurants)", 
#'                       "smoking laws (workplace)", "smoking laws (workplace; restaurant; & bar)", 
#'                       "smokers (stopped for 1 day or longer)", "smoking not allowed at work (all people)", 
#'                       "smoking not allowed in home (all people)", "smoking not allowed at work (current smokers)",
#'                       "smoking not allowed at work (former/never smokers)", "smoking not allowed in home (current smokers)", 
#'                       "smoking not allowed in home (former/never smokers)", "former smoker; ages 18+", 
#'                       "former smoker, quit 1 year+; ages 18+", "smokers (ever); ages 18+", "e-cigarette use; ages 18+",
#'                       "smokers (current); ages 18+"
#' @param race One of the following values: "all races (includes hispanic)", "white (non-hispanic)", 
#'                                          "black (non-hispanic)", "amer. indian / ak native (non-hispanic)", 
#'                                          "asian / pacific islander (non-hispanic)","hispanic (any race)"
#' @param sex Either "both sexes", "males", "females"
#' @param datatype Either "direct estimates" or "county level modeled estimates"
#' @param area A state/territory abbreviation or USA.
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' @importFrom stats setNames
#' 
#' @returns A data frame with the following columns #"State", "FIPS", "Percent", "Lower CI 95%", "Upper CI 95%", "Number of Respondents"
#' 
#' @examples
#' \dontrun{
#' risk_smoking("smoking laws (any)")
#' risk_smoking("smokers (stopped for 1 day or longer)", sex="both sexes", 
#'               datatype="county level modeled estimates", area="wa")
#' risk_smoking("smoking not allowed at work (current smokers)", sex="both sexes", 
#'               datatype="direct estimates")
#' risk_smoking("former smoker; ages 18+", sex="both sexes", 
#'               datatype="county level modeled estimates", area="ca")
#' risk_smoking("smokers (ever); ages 18+", race="hispanic (any race)", sex="both sexes", 
#'               datatype="direct estimates")
#' risk_smoking("smokers (current); ages 18+", race="all races (includes hispanic)", 
#'               sex="both sexes", datatype="county level modeled estimates", area="wa")
#' }
risk_smoking <- function(smoking, race=NULL, sex=NULL, datatype=NULL, area=NULL) {
  
  req <- create_request("risk")

  smoking_group1 = c("smoking laws (any)",
                     "smoking laws (bars)",
                     "smoking laws (restaurants)",
                     "smoking laws (workplace)",
                     "smoking laws (workplace; restaurant; & bar)")

  smoking_group2 = c("smokers (stopped for 1 day or longer)",
                     "smoking not allowed at work (all people)",
                     "smoking not allowed in home (all people)")

  smoking_group3 = c("smoking not allowed at work (current smokers)",
                     "smoking not allowed at work (former/never smokers)",
                     "smoking not allowed in home (current smokers)",
                     "smoking not allowed in home (former/never smokers)")

  smoking_group4 = c("former smoker; ages 18+",
                     "former smoker, quit 1 year+; ages 18+")
  
  smoking_group5 = c("smokers (ever); ages 18+",
                     "e-cigarette use; ages 18+")

  smoking_group6 = "smokers (current); ages 18+"
  
  #smoking group 1
  if (smoking %in% smoking_group1 && (!is.null(race) || !is.null(sex) || !is.null(sex) || !is.null(area))) {
    cli_abort("For this smoking type, Race, Sex, Datatype, and Area must ALL be NULL")
  }
  
  #smoking group 2
  if (smoking %in% smoking_group2 && !is.null(sex)) {
    if (sex == "both sexes") {
      if (datatype == "county level modeled estimates" && is.null(area)) {
        cli_abort("For county level modeled estimates on this smoking type, area must NOT be null")
      } else if (datatype == "direct estimates" && !is.null(area)) {
        cli_abort("For direct estimates for this smoking type, area must be NULL")
      } else if (is.null(datatype)) {
        cli_abort("For both sexes for this smoking group, Datatype must NOT be NULL")
      }
    } else if ((sex == "males" || sex == "females") && !is.null(area)) {
      cli_abort("For males and females for this smoking type, area must be NULL")
    }
  } else if (smoking %in% smoking_group2 && is.null(sex)) {
    cli_abort("For this smoking type, sex must NOT be NULL")
  }
  
  #smoking group 3
  if (smoking %in% smoking_group3 && !is.null(sex)) {
    if (!is.null(race) || !is.null(area)) {
      cli_abort("For all sexes in this smoking type, race and area should be NULL")
    }
  } else if (smoking %in% smoking_group3 && (is.null(sex) || is.null(datatype))) {
    cli_abort("For this smoking type, sex and dattype must NOT be NULL")
  }
  
  #smoking group 4
  if (smoking %in% smoking_group4 && ((is.null(sex) || is.null(area) || is.null(datatype)) || !is.null(race))) {
    cli_abort("For this smoking type, Sex, Datatype, and Area must not be NULL AND Race and Datatype must be NULL")
  } else if (smoking %in% smoking_group4 && datatype == "direct estimates") {
    cli_abort("For this smoking type, Datatype must be county level modeled estimates")
  }
  
  #smoking group 5
  if (smoking %in% smoking_group5 && ((is.null(race) || is.null(sex) || is.null(datatype)) || !is.null(area))) {
    cli_abort("For this smoking type, Race, Sex, and Datatype must not be NULL AND Datatype and Area must be NULL")
  } else if (smoking %in% smoking_group5 && datatype == "county level modeled estimates") {
    cli_abort("For this smoking type, Datatype must be direct estimates")
  }
  
  if (smoking %in% smoking_group6 && (is.null(race) || is.null(sex))) {
    cli_abort("For this smoking group, Race and Sex must not be NULL")
  } else if (smoking %in% smoking_group6 && (!is.null(race) && !is.null(sex)) && race == "all races (includes hispanic)") {
    if (is.null(datatype)) {
      cli_abort("For all races for this smoking type, Datatype must not be NULL")
    } else if (datatype == "direct estimates" && !is.null(area)) {
      cli_abort("For direct estimates for this smoking type, Area must be NULL")
    } else if (datatype == "county level modeled estimates" && is.null(area)) {
      cli_abort("For county level modeled estimates for this smoking type, Area must NOT be NULL")
    } 
  }
  
  resp <- req %>%
    req_url_query(
      topic="smoke",
      risk=handle_smoking(smoking),
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
  
  
  if (smoking %in% smoking_group1) {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Number of Respondents")) 
  } else if ((smoking %in% smoking_group2 || smoking %in% smoking_group3 || smoking %in% smoking_group4 
              || smoking %in% smoking_group5 || smoking %in% smoking_group6) 
              && (datatype=="direct estimates")) {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Lower CI 95%", "Upper CI 95%", "Number of Respondents")) 
  } else if ((smoking %in% smoking_group2 || smoking %in% smoking_group3 || smoking %in% smoking_group4 
              || smoking %in% smoking_group5 || smoking %in% smoking_group6) 
              && datatype=="county level modeled estimates") {
    resp %>% 
      setNames(c("County", "FIPS", "Percent", "Lower CI 95%", "Upper CI 95%")) 
  }
}
