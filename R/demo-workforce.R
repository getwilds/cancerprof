#' Access to Workforce Data
#' 
#' This function returns a data frame from Workforce in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "People Unemployed", "Rank"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' demo_workforce("WA", "county", "all races (includes hispanic)", "both sexes")
#' demo_workforce("usa", "state", "all races (includes hispanic)", "females")
#' demo_workforce("pr", "hsa", "all races (includes hispanic)", "both sexes")
#' }
demo_workforce <- function(area, areatype, race, sex) {
  
  req <- create_request("demographics")
  
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="crowd",
      demo="00012",
      race=handle_race(race),
      sex=handle_sex(sex),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    ) %>% 
    req_perform()
  
  resp <- process_response(resp)
  
  areatype_map <- c("county" = "County", "hsa" = "Health Service Area", "state" = "State")
  areatype_title <- areatype_map[areatype]
  
  resp %>% 
    setNames(c(areatype_title, "FIPS", "Percent", "People Unemployed", "Rank"))
}
