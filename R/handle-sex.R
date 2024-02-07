#' Handles Sex Values to Code
#' 
#' This function returns a matching code value for a sex for the api to use to get data from State Cancer Profiles
#'
#' @param sex Either "both sexes", "males", "females"
#' 
#' @returns A string for its respective sex
#' 
#' @examples 
#' \dontrun{
#' handle_sex("both sexes")
#' }
handle_sex <- function(sex) {
  sex <- tolower(sex)
  
  sex_mapping <- c( 
    "both sexes" = "0",
    "males" = "1",
    "females" = "2"
  )
  
  sex_code <- sex_mapping[sex]
  
  if (is_na(sex_code)) {
    stop("Invalid sex input, please check the documentation for valid inputs")
  }
  
  return(as.character(sex_code))
}
