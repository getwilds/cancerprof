#' Access to Population Data
#' 
#' This function returns a data frame from population in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cdlTools fips
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "Households", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' demo_population("WA", "county", "All Races (includes Hispanic)")
#' }

area = "wa"
areatype = "hsa"
race = "All Races (includes Hispanic)"

demo_population <- function(area, areatype, population, race=NULL, sex=NULL) {
  
  req <- create_request("demographics")
  
  if((population == "ages 40 and over" || population == "ages 50 and over") && (!is.null(race) || !is.null(sex))) {
    cli_abort("ages 40 and over and ages 50 and over, Race and Sex must be NULL")
  } else if((population == "age 18-39" || population == "age 40-64") && (is.null(sex) || !is.null(race))) {
    cli_abort("for ages 18-39 and age 40-64, Sex must not be NULL and Race must be NULL")
  } else if((population == "age under 18" || population == "ages 60 and over") && (is.null(sex) || is.null(race))) {
    cli_abort("for age under 18 and ages 60 and over, Sex and Race must not be NULL")
  } else if((population == "males" || population == "females") && (is.null(race) || !is.null(sex))) {
    cli_abort("for males, Race must not be NULL and Sex must be NULL")
  } else if(population == "foreign born" && (is.null(race) || is.null(sex))) {
    cli_abort("for foreign born, race and sex must not be null")
  } else if ((population == "american indian/alaska native" || population == "asian/pacific islander" || population == "black" ||
             population == "hispanic" || population == "non-hispanic (origin recode)" || 
             population == "white") && (is.null(sex) || !is.null(race))) {
    cli_abort("for races other than foreign born, Sex must not be NULL and race must be NULL")
  }
  

  
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="pop",
      demo=handle_population(population),
      race=handle_race(race),
      type="manyareacensus",
      sortVariableName="value",
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
  
    resp <- resp %>% 
      req_perform()
  
  process_response(resp) %>%
    setNames(c("County", "FIPS", "Percent", "Households", "Rank"))
}


demo_population("WA", "county", "foreign born", "black", "females")
