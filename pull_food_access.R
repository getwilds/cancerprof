library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)
library(cli)


req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


#' Access to Food Insecurity Data
#' 
#' This function returns a data frame from Food Insecurity in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param food Either "food insecurity" or "limited access to healthy food"
#' @param race One of the following values: "All Races (includes Hispanic)", "white non hispanic" = "01",
#'              "black (includes hispanic)","hispanic (any race)
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Value (Percent)", "People (with Limited Access"
#' 
#' @examples 
#' demo_food("WA", "county", "food insecurity", "All Races (includes Hispanic)")



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
  
  resp_lines <- resp %>% 
    resp_body_string() %>% 
    strsplit("\\n") %>%  unlist() 
  
  index_first_line_break <- which(resp_lines == "")[1]
  index_second_line_break <- which(resp_lines == "")[2]
  
  resp_lines <- resp_lines[(index_first_line_break + 1):(index_second_line_break - 1)] %>% 
    paste(collapse = "\n") %>% 
    (\(x) read.csv(textConnection(x), header=TRUE))()
  
  if (food == "limited access to healthy food") {
    resp_lines <- resp_lines %>% 
      setNames(c("County", "Percent", "People"))
  } else if (food == "food insecurity") {
    resp_lines <- resp_lines %>% 
      setNames(c("County", "Percent"))
  }
  
  resp_lines <- resp_lines %>% 
    mutate(across(everything(), ~ ifelse(is.na(.), "data not available", .))) %>%
    filter(!is.na(County) & County != "" & (!is.na(Percent) | !is.na(People)))
  
  return(resp_lines)
}


demo_food("WA", "food insecurity", "black")
