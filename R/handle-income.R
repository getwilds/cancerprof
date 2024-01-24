#' Handles Income Values to Code
#' 
#' This function returns a matching code value for a Income for the api to use to get data from State Cancer Profiles
#'
#' @param income Either "median family income" or "median household income"
#' 
#' @returns A string for its respective income
#' 
#' @examples 
#' \dontrun{
#' handle_income("median family income")
#' }
handle_income <- function(income) {
  income <- tolower(income)
  
  income_mapping <- c(
    "median family income" = "00010",
    "median household income" = "00011"
  )
  
  income_code <- income_mapping[income]
  
  if (is.null(income_code)) {
    stop("Invalid input")
  }
  
  return(as.character(income_code))
}
