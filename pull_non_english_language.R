library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)

req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


#' Access to Non-English Language
#' 
#' This function returns a data frame from Crowding in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' 
#' @returns A data frame with the following columns "County", "Value (Percent)", "Households (Language Isolation)", "Rank within US (of 3142 counties)"
#' 
#' @examples 
#' demo_language("WA", "county")


demo_language <- function(area, areatype) {
  resp <- req %>%  
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="lang",
      demo="00015",
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
    setNames(c("County", "FIPS", "Percent", "Households", "Rank")) %>% 
    filter(str_detect(County, "County")) 
  
}


demo_language("WA", "county")
