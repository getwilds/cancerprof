#' Access to Poverty Data
#' 
#' This function returns a data frame from Poverty in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param poverty Either "families below poverty", "persistent poverty", "persons below poverty", "persons < 150% of poverty"
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "Households", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' demo_poverty("WA", "county", "persistent poverty")
#' demo_poverty("usa", "state", "families below poverty", "black")
#' demo_poverty("dc", "hsa", "families below poverty", "All Races (includes Hispanic)")
#' }
demo_poverty <- function(area, areatype, poverty, race=NULL, sex=NULL) {
  
  req <- create_request("demographics")
  
  if (poverty == "persistent poverty" && (areatype == "hsa" || areatype == "state")) {
    cli_abort("For persistent poverty, areatype must be county")
  }
  if ((poverty == "persistent poverty" || poverty == "persons < 150% of poverty") && (!is.null(race) || !is.null(sex))) {
    cli_abort("for persistent poverty and persons < 150% of poverty, Race and Sex must be NULL")
  } else if((poverty == "families below poverty") && (!is.null(sex) || is.null(race))) {
    cli_abort("for families below poverty, Sex must be NULL and Race must not be NULL")
  } else if((poverty == "persons below poverty") && (is.null(sex) || is.null(race))) {
    cli_abort("for persons below poverty, Sex and Race must not be NULL")
  }
  
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="pov",
      demo=handle_poverty(poverty),
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
  
  if (poverty == "persistent poverty") {
    resp %>% 
      setNames(c(areatype_title, "FIPS", "Persistent Poverty"))
  } else {
    resp %>%
      setNames(c(areatype_title, "FIPS", "Percent", "People", "Rank"))
  }
}
