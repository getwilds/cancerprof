library(httr2)
library(dplyr)
library(cdlTools)
library(cli)

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
#' @examples 
#' demo_income("wa", "county", "median family income", "all races (includes hispanic)")

handle_income <- function(income) {
  income <- tolower(income)
  
  income_mapping <- c(
    "median family income" = "00010",
    "median household income" = "00011"
  )
  
  income_code <- income_mapping[income]
  
  if (is.null(income_code)) {
    stop("Invalid input")
  }
  
  return(as.character(income_code))
}

demo_income <- function(area, areatype, income, race) {
  
  req <- create_request()
  
  resp <- req %>%  
    req_url_query(
      stateFIPS=fips(area),
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
    setNames(c("County", "FIPS", "Dollars", "Rank"))
  
  resp$Dollars <- as.integer((resp$Dollars))
  
  resp
}

demo_income("wa", "county", "median family income", "all races (includes hispanic)")