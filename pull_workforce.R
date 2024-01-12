library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)

req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


#' Access to Workforce Data
#' 
#' This function returns a data frame from Workforce in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#' 
#' @returns A data frame with the following columns "County", "FIPS", Value (Percent)", "People Unemployed", "Rank within US (of 3143 counties)"
#' 
#' @examples 
#' demo_workforce("WA", "county", "All Races (includes Hispanic)", "both sexes")



handle_race <- function(race) {
  race <- tolower(race)
  
  race_mapping <- c(
    "all races (includes hispanic)" = "00",
    "white (includes hispanic)" = "01",
    "white non-hispanic" = "07",
    "black" = "02",
    "amer. indian/alaskan native (includes hispanic)" = "03",
    "all ages, asian or pacific islander (includes hispanic)" = "04",
    "hispanic (any race)" = "05"
  )
  
  code <- race_mapping[race]
  
  if (is.null(code)) {
    stop("Invalid input")
  }
  
  return(as.character(code))
}


handle_sex <-function(sex) {
  sex <- tolower(sex)
  
  sex_mapping <- c( 
    "both sexes" = "0",
    "males" = "1",
    "females" = "2"
  )
  
  sex_code <- sex_mapping[sex]
  
  if (is.null(sex_code)) {
    stop("Invalid input")
  }
  
  return(as.character(sex_code))
}


demo_workforce <- function(area, areatype, race, sex) {
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="crowd",
      demo="00012",
      race=handle_race(race),
      sex=handle_sex(sex),
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
    setNames(c("County", "FIPS", "Percent", "People Unemployed", "Rank")) %>% 
    filter(str_detect(County, "County")) 
  
}


demo_workforce("WA", "county", "all races (includes hispanic)", "both sexes")
