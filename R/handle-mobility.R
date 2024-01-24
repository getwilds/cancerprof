#' Handles Mobility Values to Code
#' 
#' This function returns a matching code value for a mobility for the api to use to get data from State Cancer Profiles
#'
#' @param sex Either "i haven't moved (in past year)", "moved from outside us (in past year)",
#'            "moved, different state (in past year)", "moved, different county, same state (in past year)",
#'            "moved, same county (in past year)"
#' 
#' @returns A string for its respective mobility
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
  
  if (is.null(mobility_code)) {
    stop("Invalid input")
  }
  
  return(as.character(mobility_code))
}
