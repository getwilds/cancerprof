#' Access to Non-English Language
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "Households", "Rank"
#' 
#' @examples 
#' demo_language("WA", "county")
demo_language <- function(area, areatype) {
  
  req <- create_request("demographics")
  
  resp <- req %>%  
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="lang",
      demo="00015",
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    ) %>% 
    req_perform()
  
  
  resp <- process_response(resp)
  
  if (areatype == "county") {
    resp %>% 
      setNames(c("County", "FIPS", "Percent", "Households", "Rank")) 
  } else if (areatype == "hsa") {
    resp %>% 
      setNames(c("Health Service Area", "FIPS", "Percent", "Households", "Rank"))
  } else if (areatype == "state") {
    resp %>% 
      setNames(c("State", "FIPS", "Percent", "Households", "Rank"))
  }
}


demo_language("usa", "hsa")
