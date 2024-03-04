#' Access to Population Data
#' 
#' This function returns a data frame containing demographic data on Population in State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param population One of the following values: "age under 18", "age 18-39", "age 40-64", "ages 40 and over", "ages 50 and over", "ages 60 and over",
#'                                                "american indian/alaska native", "asian/pacific islander", "black", "foreign born", "hispanic", 
#'                                                "non-hispanic (origin recode)", "white", "males", "females"
#' @param race One of the following values: "american indian/alaska native", "asian/pacific islander", 
#'                                          "black", "hispanic", "white (includes hispanic)", "white non-hispanic", "hispanic (any race)"
#' @param sex Either "both sexes", "males", "females"
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' @importFrom stats setNames
#' 
#' @returns A data frame with the following columns: Area Type, Area Code, "Percent", "Households", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' demo_population(area = "WA",
#'                 areatype = "county",
#'                 population = "males",
#'                 race = "all races (includes hispanic)")
#'                 
#' demo_population(area = "dc",
#'                 areatype = "hsa",
#'                 population = "foreign born",
#'                 race = "black",
#'                 sex = "females")
#'                 
#' demo_population(area = "usa",
#'                 areatype = "state",
#'                 population = "foreign born",
#'                 race = "hispanic (any race)", 
#'                 sex = "females")
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
    cli_abort("for foreign born, race and sex must not be NULL")
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
    
    areatype_map <- c("county" = "County", "hsa" = "Health_Service_Area", "state" = "State")
    areatype_title <- areatype_map[areatype]
    
    areacode_map <- c("county" = "FIPS", "state" = "FIPS", "hsa" = "HSA_Code")
    areacode_title <- areacode_map[areatype]
    
    resp %>% 
      setNames(c(areatype_title, areacode_title, "Percent", "Households", "Rank"))
}
