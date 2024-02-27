#' Handles Mobility Values to Code
#' 
#' This function returns a matching code value for a mobility for the api to use to get data from State Cancer Profiles
#'
#' @param mobility Either "i haven't moved (in past year)", "moved from outside us (in past year)",
#'            "moved, different state (in past year)", "moved, different county, same state (in past year)",
#'            "moved, same county (in past year)"
#' 
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective mobility
#' 
#' @noRd
#' 
#' @examples 
#' \dontrun{
#' handle_mobility("i haven't moved (in past year)")
#' }
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
  
  if (is_na(mobility_code)) {
    stop("Invalid mobility input, please check the documentation for valid inputs")
  }
  
  return(as.character(mobility_code))
}
