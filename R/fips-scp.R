#' Process State to FIPS
#' 
#' This function processes state names and abbreviations to FIPS data
#'
#' @param area A state name or state abbreviation or United States of America or United States or usa or us
#' 
#' @importFrom cdlTools fips
#' @importFrom stringr str_pad
#' 
#' @returns A string fips value or NA
#' 
#' @examples
#' \dontrun{
#' fips_scp("pr")
#' fips_scp("ca")
#' fips_scp("usa")
#' }
fips_scp <- function(area, topic="Demographics") {
  
  area = tolower(area)
  
  usa_list = c("united states", "united states of america", "us", "usa")
  
  
  if (area %in% usa_list) {
    return("00")
  } else {
    str_pad(fips(area), width=2, side="left", pad="0")
  }
}
