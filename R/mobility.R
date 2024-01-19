#' Access to Mobility Data
#' 
#' This function returns a data frame from mobility in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param mobility One of five choices from "i haven't moved (in past year)", "moved from outside us (in past year)",
#'                  "moved, different state (in past year)", "moved, different county, same state (in past year)",
#'                  "moved, same county (in past year)"
#'                  
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "People", "Rank"
#' 
#' \dontrun{
#' @examples 
#' demo_mobility("WA", "county", "moved, different county, same state (in past year)")
#' }
demo_mobility <- function(area, areatype, mobility) {
  
  req <- create_request("demographics")
  
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="mob",
      demo=handle_mobility(mobility),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    ) %>% 
    req_perform()

    resp <- process_response(resp)
    
    if (areatype == "county") {
      resp %>% 
        setNames(c("County", "FIPS", "Percent", "People", "Rank")) 
    } else if (areatype == "hsa") {
      resp %>% 
        setNames(c("Health Service Area", "FIPS", "Percent", "People", "Rank"))
    } else if (areatype == "state") {
      resp %>% 
        setNames(c("State", "FIPS", "Percent", "People", "Rank"))
    }
}

demo_mobility("WA", "county", "moved, different county, same state (in past year)")
demo_mobility("usa", "hsa", "moved, same county (in past year)") ##???? Whats going on here
