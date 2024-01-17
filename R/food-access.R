library(httr2)
library(cdlTools)
library(cli)
library(dplyr)

#' Access to Food Insecurity Data
#' 
#' This function returns a data frame from Food Insecurity in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param food Either "food insecurity" or "limited access to healthy food"
#' @param race One of the following values: "All Races (includes Hispanic)", "white non hispanic" = "01",
#'              "black (includes hispanic)","hispanic (any race)
#'              
#' @importFrom httr2 req_url_query, req_perform
#' @importFrom cdlTools fip
#' @importFrom cli cli_abort
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Value", "People"
#' 
#' @export
#' 
#' @examples 
#' demo_food("WA", "county", "food insecurity", "All Races (includes Hispanic)")


handle_food <- function(food) {
  food <- tolower(food)
  
  food_mapping <- c(
    "food insecurity" = "03003",
    "limited access to healthy food" = "03004"
  )
  
  food_code <- food_mapping[food]
  
  if (is.null(food_code)) {
    stop("Invalid input")
  }
  
  return(as.character(food_code))
}

demo_food <- function(area, food, race=NULL) {
  
  req <- create_request()
  
  if (food == "limited access to healthy food" && !is.null(race)) {
    cli_abort("For limited access to healthy food, Race must be NULL.")
  } else if (food == "food insecurity" && is.null(race)) {
    cli_abort("For food insecurity, Race must NOT be NULL.")
  }
  
  req_draft <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype="county",
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
  
  resp <- process_response(resp)
  
  if (food == "limited access to healthy food") {
    resp <- resp %>%
      setNames(c("County", "FIPS", "Percent", "People"))
  } else if (food == "food insecurity") {
    resp <- resp %>%
      setNames(c("County", "FIPS", "Percent"))
  }
  
  resp$Percent <- as.integer((resp$Percent))
  
  resp
}

demo_food("WA", "food insecurity", "black")
