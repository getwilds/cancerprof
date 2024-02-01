#' Handles Cancer Incidence Year Values to Code
#' 
#' This function returns a matching code value for a Cancer Incidence Year for the api to use to get data from State Cancer Profiles
#'
#' @param year Either "latest 5 year average", "latest single year (us by state)"
#' 
#' @returns A string for its respective Cancer Incidence Year
#' 
#' @examples 
#' \dontrun{
#' handle_year("latest 5 year average")
#' }
handle_year <-function(year) {
  year <- tolower(year)
  
  year_mapping <- c(
    "latest 5 year average" = "0",
    "latest single year (us by state)" = "1"
  )
  
  year_code <- year_mapping[year]
  
  if (is.null(year_code)) {
    stop("Invalid input")
  }
  
  return(as.character(year_code))
}
