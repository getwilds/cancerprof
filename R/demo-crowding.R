#' Access to Crowding Data
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param crowding "household with >1 person per room"
#' @param race One of the following values:
#'             "All Races (includes Hispanic)",
#'             "white (includes hispanic)",
#'             "white non-hispanic",
#'             "black",
#'             "amer. indian/alaskan native (includes hispanic)",
#'             "asian or pacific islander (includes hispanic)",
#'             "hispanic (any race)"
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' 
#' @returns A data frame with the following columns Area, Area Code, "Percent", "Households", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' demo_crowding(area = "WA", 
#'               areatype = "hsa",
#'               crowding = "household with >1 person per room",
#'               race = "All Races (includes Hispanic)")
#'              
#' demo_crowding(area = "usa",
#'               areatype = "state",
#'               crowding = "household with >1 person per room",
#'               race = "All Races (includes Hispanic)")
#'
#' demo_crowding(area = "pr",
#'               areatype = "hsa",
#'               crowding = "household with >1 person per room",
#'               race = "black")
#' }
#' 
demo_crowding <- function(area, areatype, crowding, race) {

  req <- create_request("demographics")
  
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="crowd",
      demo=handle_crowding(crowding),
      race=handle_race(race),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    ) %>% 
    req_perform()
  
  resp <- process_response(resp)
  
  areatype_map <- c("county" = "County", "hsa" = "Health_Service_Area", "state" = "State")
  areacode_map <- c("county" = "FIPS", "state" = "FIPS", "hsa" = "HSA_Code")
  
  areatype_title <- areatype_map[areatype]
  areacode_title <- areacode_map[areatype]
  
  resp %>% 
    setNames(c(areatype_title, areacode_title, "Percent", "Households", "Rank"))
}

area = "wa"
areatype = "county"
race = "all races (includes hispanic)"
crowding = "household with >1 person per room"
