#' Access to Population Data
#' 
#' This function returns a data frame from population in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param population One of the following values: "age under 18", "age 18-39", "age 40-64", "ages 40 and over", "ages 50 and over", "ages 60 and over",
#'                                                "american indian/alaska native", "asian/pacific islander", "black", "foreign born", "hispanic", 
#'                                                "non-hispanic (origin recode)", "white", "males", "females"
#' @param race One of the following values: "american indian/alaska native", "asian/pacific islander", 
#'                                          "black", "foreign born", "hispanic", "non-hispanic (origin recode)", "white"
#' @param sex Either "both sexes", "male", "female"
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "Households", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' demo_population("WA", "county", "asian/pacific islander", sex="females")
#' demo_population("dc", "hsa", "foreign born", "black", "females")
#' demo_population("usa", "state", "foreign born", "black", "females")
#' 
#' }
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
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="pop",
      demo=handle_population(population),
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
  
    resp <- process_response(resp)
    
    areatype_map <- c("county" = "County", "hsa" = "Health Service Area", "state" = "State")
    areatype_title <- areatype_map[areatype]
    
    resp %>% 
      setNames(c(areatype_title, "FIPS", "Percent", "Households", "Rank"))
}