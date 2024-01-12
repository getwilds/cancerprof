library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)

req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


#' Access to Mobility Data
#' 
#' This function returns a data frame from mobility in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param mobility One of five choices from "i haven't moved (in past year)", "moved from outside us (in past year)",
#'                  "moved, different state (in past year)", "moved, different county, same state (in past year)",
#'                  "moved, same county (in past year)"
#' 
#' @returns A data frame with the following columns "County", "Value (Percent)", "People", "Rank within US (of 3142 counties)"
#' 
#' @examples 
#' demo_mobility("WA", "county", "moved, same county (in past year)")



handle_mobility <-function(mobility) {
  mobility <- tolower(mobility)
  
  mobility_mapping <- c( 
    "i haven't moved (in past year)" = "00017",
    "moved from outside us (in past year)" = "00021",
    "moved, different state (in past year)" = "00020",
    "moved, different county, same state (in past year)" = "00019",
    "moved, same county (in past year)" = "00018"
  )
  
  mobility_code <- mobility_mapping[mobility]
  
  if (is.null(mobility_code)) {
    stop("Invalid input")
  }
  
  return(as.character(mobility_code))
}



demo_mobility <- function(area, areatype, mobility) {
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype=tolower(areatype),
      topic="mob",
      demo=handle_mobility(mobility),
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


demo_mobility("WA", "county", "moved, different county, same state (in past year)")
