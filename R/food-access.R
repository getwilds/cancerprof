#' Access to Food Insecurity Data
#' 
#' This function returns a data frame from Food Insecurity in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "state"
#' @param food Either "food insecurity" or "limited access to healthy food"
#' @param race One of the following values: "All Races (includes Hispanic)", "white non hispanic" = "01",
#'              "black (includes hispanic)","hispanic (any race)
#'              
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Value", "People"
#' 
#' @export
#' 
#'
#' @examples
#' \dontrun{
#' demo_food("wa", "county", "food insecurity", "black")
#' demo_food("usa", "state", "limited access to healthy food")
#' demo_food("pr", "county", "food insecurity", "all races (includes hispanic)")
#' }
demo_food <- function(area, areatype, food, race=NULL) {
  
  req <- create_request("demographics")
  
  if (food == "limited access to healthy food" && !is.null(race)) {
    cli_abort("For limited access to healthy food, Race must be NULL.")
  } else if (food == "food insecurity" && is.null(race)) {
    cli_abort("For food insecurity, Race must NOT be NULL.")
  }
  
  req_draft <- req %>% 
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="food",
      demo=handle_food(food),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    )
  
  if(!is.null(race)) {
    req_draft <- req_draft %>% 
      req_url_query(race=handle_race(race))
  }
  
  resp <- req_draft %>%
    req_perform() 
  
  resp <- process_response(resp) %>% 
    mutate(Value..Percent. = as.integer(Value..Percent.))  
  
  areatype_map <- c("county" = "County", "state" = "State")
  areatype_title <- areatype_map[areatype]
  
  if (food == "limited access to healthy food") {
    resp %>% 
      setNames(c(areatype_title, "FIPS", "Percent", "People"))
  } else if (food == "food insecurity"){
    resp %>%
      setNames(c(areatype_title, "FIPS", "Percent"))
  }
}
