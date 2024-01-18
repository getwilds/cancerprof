#' Access to Crowding Data
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
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
#' demo_crowding("WA", "county", "All Races (includes Hispanic)")
#' }


area = "wa"
areatype = "hsa"
race = "All Races (includes Hispanic)"

demo_crowding <- function(area, areatype, race) {

  req <- create_request("demographics")
  
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="crowd",
      demo="00027",
      race=handle_race(race),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    ) %>% 
    req_perform()
  
  process_response(resp) %>%
    setNames(c("County", "FIPS", "Percent", "Households", "Rank"))
}
demo_crowding("WA", "county", "All Races (includes Hispanic)")
