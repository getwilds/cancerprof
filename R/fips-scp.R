#' Process State to FIPS
#' 
#' This function processes state names and abbreviations to FIPS data
#'
#' @param area A state name or state abbreviation or United States of America or United States or usa or us
#' 
#' @importFrom cdlTools fips
#' 
#' @returns A string fips value or NA
#' 
#' @examples
#' \dontrun{
#' fips_scp("pr")
#' fips_scp("usa")
#' }
fips_scp <- function(area) {
  us <- "United States"
  usa <- "United States of America"
  us1 <- "us"
  usa1 <- "usa"
  if (area == tolower(us) || area == tolower(usa) || area == tolower(us1) || area == tolower(usa1)) {
    return("00")
  } else {
    fips(area)
  }
}

fips_scp("wa")
