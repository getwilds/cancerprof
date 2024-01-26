#' Access to Non-English Language
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "Households", "Rank"
#' 
#' @examples 
#' demo_language("WA", "county")
#' demo_language("dc", "hsa")
#' demo_language("usa", "state")
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
  
  areatype_map <- c("county" = "County", "hsa" = "Health Service Area", "state" = "State")
  areatype_title <- areatype_map[areatype]
  
  resp %>% 
    setNames(c(areatype_title, "FIPS", "Percent", "Households", "Rank"))
}
