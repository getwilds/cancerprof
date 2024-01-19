#' Access to Income Data
#' 
#' This function returns a data frame from Income in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param income Either "median family income" or "median household income"
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#'              
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cdlTools fips
#' @importFrom cli cli_abort
#'  
#' @returns A data frame with the following columns "County", "FIPS", "Dollars", "Rank"
#' 
#' @export
#' 
#' \dontrun{
#' @examples 
#' demo_income("wa", "county", "median family income", "all races (includes hispanic)")
#' demo_income("usa", "state", "median family income", "all races (includes hispanic)")
#' }

demo_income <- function(area, areatype, income, race) {
  
  req <- create_request("demographics")
  
  resp <- req %>%  
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="inc",
      demo=handle_income(income),
      race=handle_race(race),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    ) %>% 
    req_perform() 
  
  resp <- process_response(resp) %>% 
    mutate(Value..Dollars. = as.integer(Value..Dollars.))  
  
  if (areatype == "county") {
    resp %>% 
      setNames(c("County", "FIPS", "Dollars", "Rank")) 
  } else if (areatype == "hsa") {
    resp %>% 
      setNames(c("Health Service Area", "FIPS", "Dollars", "Rank"))
  } else if (areatype == "state") {
    resp %>% 
      setNames(c("State", "FIPS", "Dollars", "Rank"))
  }
}

demo_income("usa", "state", "median family income", "all races (includes hispanic)")
