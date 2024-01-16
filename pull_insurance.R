library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)
library(cli)


req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


#' Access to Insurance Data
#' 
#' This function returns a data frame from Insurance in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param insurance Either 
#'                  "% Insured in demographic group, all income levels", "% Insured in demographic group, people at or below 138% of Poverty"
#'                  "% Insured in demographic group, people at or below 200% of Poverty", "% Insured in demographic group, people at or below 250% of Poverty"
#'                  "% Insured in demographic group, people at or below 400% of Poverty","% Insured in demographic group, people between 138% - 400% of poverty"
#'                  "% uninsured in demographic group, all income levels","% uninsured in demographic group, people at or below 138% of Poverty"
#'                  "% uninsured in demographic group, people at or below 200% of Poverty","% uninsured in demographic group, people at or below 250% of Poverty"
#'                  "% uninsured in demographic group, people at or below 400% of Poverty","% uninsured in demographic group, people between 138% - 400% of poverty"
#'@param sex Either "both sexes", "male", "female"
#'@param age Either "under 19 years", "18 to 64 years","21 to 64 years","40 to 64 years","50 to 64 years","under 65 years" for "both sexes"
#'                  "18 to 64 years","40 to 64 years","50 to 64 years","Under 65 years" for "males" and "females"
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "People", "Rank"
#' 
#' @examples 
#' demo_insurance("WA", "county", "both sexes", "under 19 years")



handle_insurance <- function(insurance) {
  insurance <- tolower(insurance)
  
  insurance_mapping <- c(
    "% Insured in demographic group, all income levels" = "00030",
    "% Insured in demographic group, people at or below 138% of Poverty" = "00033",
    "% Insured in demographic group, people at or below 200% of Poverty" = "00031",
    "% Insured in demographic group, people at or below 250% of Poverty" = "00032",
    "% Insured in demographic group, people at or below 400% of Poverty" = "00034",
    "% Insured in demographic group, people between 138% - 400% of poverty" = "00035",
    "% uninsured in demographic group, all income levels" = "00040",
    "% uninsured in demographic group, people at or below 138% of Poverty" = "00043",
    "% uninsured in demographic group, people at or below 200% of Poverty" = "00041",
    "% uninsured in demographic group, people at or below 250% of Poverty" = "00042",
    "% uninsured in demographic group, people at or below 400% of Poverty" = "00044",
    "% uninsured in demographic group, people between 138% - 400% of poverty" = "00045"
    )
  
  insurance_code <- insurance_mapping[insurance]
  
  if (is.null(insurance_code)) {
    stop("Invalid input")
  }
  
  return(as.character(insurance_code))
}


handle_age <- function(age) {
  age <- tolower(age)
  
  age_mapping <- c(
    "under 19 years" = "175",
    "18 to 64 years" = "174",
    "21 to 64 years" = "176",
    "40 to 64 years" = "122",
    "50 to 64 years" = "141",
    "under 65 years" = "006"
  )
  
  age_code <- age_mapping[age]
  
  if (is.null(age_code)) {
    stop("Invalid input")
  }
  
  return(as.character(age_code))
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

demo_insurance <- function(area, areatype, insurance, sex, age) {
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="ins",
      demo=handle_insurance(insurance),
      race="00",
      sex=handle_sex(sex),
      age=handle_age(age),
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
    setNames(c("County", "FIPS", "Percent", "People", "Rank")) %>% 
    filter(str_detect(County, "County")) 
  
}


demo_insurance("WA", "county", "% Insured in demographic group, all income levels",  "both sexes", "under 19 years")
