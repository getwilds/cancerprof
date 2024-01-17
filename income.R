library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)
library(cli)


req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


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
#' @returns A data frame with the following columns "County", "FIPS", "Dollars", "Rank within US (of 3143 counties)"
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


handle_race <- function(race) {
  race <- tolower(race)
  
  race_mapping <- c(
    "all races (includes hispanic)" = "00",
    "white (includes hispanic)" = "01",
    "white non-hispanic" = "07",
    "black" = "02",
    "amer. indian/alaskan native (includes hispanic)" = "03",
    "asian or pacific islander (includes hispanic)" = "04",
    "hispanic (any race)" = "05"
  )
  
  race_code <- race_mapping[race]
  
  if (is.null(race_code)) {
    stop("Invalid input")
  }
  
  return(as.character(race_code))
}



demo_income <- function(area, areatype, income, race) {
  
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
  
  resp_lines <- resp %>% 
    resp_body_string() %>% 
    strsplit("\\n") %>%  unlist() 
  
  index_first_line_break <- which(resp_lines == "")[1]
  index_second_line_break <- which(resp_lines == "")[2]
  
  resp_lines[(index_first_line_break + 1):(index_second_line_break -1)] %>% 
    paste(collapse = "\n") %>% 
    (\(x) read.csv(textConnection(x), header=TRUE))() %>% 
    setNames(c("County", "FIPS", "Dollars", "Rank")) %>% 
    filter(str_detect(County, "County")) 
  
}


demo_income("wa", "county", "median family income", "all races (includes hispanic)")


