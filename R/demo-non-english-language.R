#' Access to Non-English Language
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param language "language isolation"
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
#'               areatype = "county",
#'               language = "language isolation")
#'               
#' demo_language(area = "dc",
#'               areatype = "hsa",
#'               language = "language isolation")
#'               
#' demo_language(area = "usa", 
#'               areatype = "state",
#'               language = "language isolation")
demo_language <- function(area, areatype, language) {
  
  req <- create_request("demographics")
  
  resp <- req %>%  
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="lang",
      demo=handle_non_english(language),
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
