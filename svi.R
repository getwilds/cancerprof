library(httr2)
library(tidyverse)
library(dplyr)
library(cdlTools)
 
req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")


#' Access to Social Vulnerability Index (SVI) Data
#' 
#' This function returns a data frame from Social Vulnerability Index (SVI) in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @params social Either "Overall, "socioeconomic status", "household characteristics", "racial & ethinic minority status", "housing type & transportation"
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Score"
#' @examples 
#' demo_svi("WA", "overall")



handle_svi <- function(svi) {
  svi <- tolower(svi)
  
  svi_mapping <- c(
    "overall" = "03010",
    "socioeconomic status" = "03011",
    "household characteristics" = "03012",
    "racial & ethinic minority status" = "03013",
    "housing type & transportation" = "03014"
  )
  
  svi_code <- svi_mapping[svi]
  
  if (is.null(svi_code)) {
    stop("Invalid input")
  }
  
  return(as.character(svi_code))
}



demo_svi <- function(area, svi) {
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips(area),
      areatype="county",
      topic="svi",
      demo=handle_svi(svi),
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
    setNames(c("County", "FIPS", "Score"))
  
}


demo_svi("WA", "overall")
