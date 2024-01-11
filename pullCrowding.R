library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)

req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


#' Access to Crowding Data
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param race One of the following values: "All Races (includes Hispanic)", ...
#' 
#' @returns A data frame with the following columns "County", "Value (Percent)", "Households (with >1 Person Per Room)", "Rank within US (of 3143 counties)"
#' 
#' @examples 
#' demo_crowding("WA", "county", "All Races (includes Hispanic)")



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



demo_crowding <- function(area, areatype, race) {
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="crowd",
      demo="00027",
      race=handle_race(race),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="asc",
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
    setNames(c("County", "FIPS", "Percent", "Households", "Rank")) %>% 
    filter(str_detect(County, "County")) 
  
}


demo_crowding("WA", "county", "black")
