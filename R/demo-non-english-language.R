#' Access to Non-English Language
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' 
#' @returns A data frame with the following columns: Area Type, Area Code, "Percent", "Households", "Rank"
#' 
#' @export
#' 
#' @examples 
#' demo_language(area = "WA", 
#'               areatype = "county")
#'               
#' demo_language(area = "dc",
#'               areatype = "hsa")
#'               
#' demo_language(area = "usa", 
#'               areatype = "state")
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
  
  areatype_map <- c("county" = "County", "hsa" = "Health_Service_Area", "state" = "State")
  areatype_title <- areatype_map[areatype]
  
  areacode_map <- c("county" = "FIPS", "state" = "FIPS", "hsa" = "HSA_Code")
  areacode_title <- areacode_map[areatype]
  
  resp %>% 
    setNames(c(areatype_title, areacode_title, "Percent", "Households", "Rank"))
}
