#' Get area titles and codes based on area type
#'
#' This function returns the title and code corresponding to the specified area type.
#'
#' @param areatype One of the following values:
#' - "county"
#' - "hsa"
#' - "state".
#' @return A character vector containing the title and code of the specified area type.
#' @examples
#' get_area("county")
#' get_area("hsa")


get_area <- function(areatype) {
  areatype_map <- c("county" = "County", "hsa" = "Health_Service_Area", "state" = "State")
  areacode_map <- c("county" = "FIPS", "state" = "FIPS", "hsa" = "HSA_Code")
  
  areatype_title <- areatype_map[areatype]
  areacode_title <- areacode_map[areatype]
  
  return(c(areatype_title, areacode_title))
}