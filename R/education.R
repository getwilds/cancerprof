library(httr2)
library(dplyr)
library(cdlTools)
library(cli)


#' Access to Education Data
#' 
#' This function returns a data frame from Education in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' 
#' @returns A data frame with the following columns "County", "Value (Percent)", "Households (with >1 Person Per Room)", "Rank within US (of 3143 counties)"
#' 
#' @examples 
#' demo_crowding("WA", "county", "All Races (includes Hispanic)")

demo_education <- function(area, areatype, education, race=NULL, sex=NULL) {
  
  req <- create_request()
  
  if(education == "less than 9th grade" && (!is.null(race) || !is.null(sex))) {
    cli_abort("For Less than 9th Grade, Race and Sex must be NULL.")
  } else if (education == "at least high school" && !is.null(race) && is.null(sex)) {
    cli_abort("For At Least High School, Race must be NULL and Sex must be NOT NULL.")
  } else if (education == "at least bachelors degree" && (is.null(race) || is.null(sex))) {
    cli_abort("For At Least Bachelors Degree, Race and Sex must be NOT NULL.")
  }
  
  
  req_draft <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="ed",
      demo=handle_education(education),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    )
    
    if(!is.null(race)) {
      req_draft <- req_draft %>% 
        req_url_query(race=handle_race(race))
    }
    
    if(!is.null(sex)) {
      req_draft <- req_draft %>% 
        req_url_query(sex=handle_sex(sex))
    }
  
    resp <- req_draft %>% 
      req_perform() 
  
  resp <- process_response(resp) %>% 
    setNames(c("County", "FIPS", "Percent", "People", "Rank"))
  resp
}


demo_education("wa", "county", "at least high school", sex="males")


