library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)
library(cli)


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


handle_education <-function(education) {
  education <- tolower(education)
  
  edu_mapping <- c( 
    "less than 9th grade" = "00004",
    "at least high school" = "00109",
    "at least bachelors degree" = "00006"
  )
  
  edu_code <- edu_mapping[education]
  
  if (is.null(edu_code)) {
    stop("Invalid input")
  }
  
  return(as.character(edi_code))
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


demo_education <- function(area, areatype, education, race=NULL, sex=NULL) {
  
  if(education == "less than 9th grade" && (!is.null(race) || !is.null(sex))) {
    cli_abort("For Less than 9th Grade, Race and Sex must be NULL.")
  } else if (education == "at least high school" && !is.null(race) && is.null(sex)) {
    cli_abort("For At Least High School, Race must be NULL and Sex must be NOT NULL.")
  } else if (education == "at least bachelors degree" && (is.null(race) || is.null(sex))) {
    cli_abort("For At Least Bachelors Degree, Race and Sex must be NOT NULL.")
  }
  
  
  req_draft <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="ed",
      demo=handle_education(education),
      
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    )
    
    if(!is.null(race)) {
      req_draft <- req_draft %>% 
        req_url_query(race=handle_race(race))
    }
    
    if(!is.null(sex)) {
      req_draft <- req_draft %>% 
        req_url_query(sex=handle_sex(sex))
    }
  
    resp <- req_draft %>% 
      req_perform() 
  
  resp_lines <- resp %>% 
    resp_body_string() %>% 
    strsplit("\\n") %>%  unlist() 
  
  index_first_line_break <- which(resp_lines == "")[1]
  index_second_line_break <- which(resp_lines == "")[2]
  
  resp_lines[(index_first_line_break + 1):(index_second_line_break -1)] %>% 
    paste(collapse = "\n") %>% 
    (\(x) read.csv(textConnection(x), header=TRUE))() %>% 
    setNames(c("County", "FIPS", "Percent", "People", "Rank")) %>% 
    filter(str_detect(County, "County")) 
  
}


demo_education("wa", "county", "at least high school", sex="males")


