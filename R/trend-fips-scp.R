#' Process State to FIPS for Trend Data
#'
#' This function processes state names and abbreviations to FIPS data need for
#' accessing trend data
#'
#' @param area A state name or state abbreviation or
#' United States of America or United States or usa or us
#' or "seer 9 registeries"
#'
#' @importFrom stats setNames
#' @importFrom cdlTools fips
#' @importFrom stringr str_pad
#'
#' @returns A string fips value or NA
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' trend_fips_scp("seer 9 registeries")
#' trend_fips_scp("ca")
#' trend_fips_scp("washington")
#' trend_fips_scp("usa")
#' }
trend_fips_scp <- function(area) {
  
  state_abbreviations <- setNames(tolower(state.abb), tolower(state.name))
  abbreviation_to_name <- setNames(names(state_abbreviations), state_abbreviations)
  
  usa_list <- c("united states", "united states of america", "us", "usa")
  
  state_prefixes <- c(
    "utah" = "26",
    "new york" = "46",
    "new mexico" = "23",
    "new jersey" = "44",
    "massachusetts" = "47",
    "louisiana" = "43",
    "kentucky" = "42",
    "iowa" = "22",
    "idaho" = "45",
    "hawaii" = "21",
    "connecticut" = "02",
    "california" = "97"
  )
  
  area <- tolower(area)  # Convert input to lowercase
  
  if (area %in% usa_list) {
    return("9900")
    
  } else if (area == "seer 9 registeries") {
    return("5099")
  
    #checks for state name as area
  } else if (area %in% names(state_prefixes)) {
    prefix <- state_prefixes[area]
    
    #checks for state abbr as area and if state abbr is in state prefixes
  } else if (area %in% state_abbreviations && abbreviation_to_name[[area]] %in% names(state_prefixes)) {
    state_name <- abbreviation_to_name[area]
    
    prefix <- state_prefixes[[state_name]]
    
  } else {
    prefix <- "99"
  }
  
  state_name <- fips(area)
  fips_code <- str_pad(fips(state_name), width = 2, side = "left", pad = "0")
  fips_output <- paste0(prefix, fips_code)
  
  return(fips_output)
}
