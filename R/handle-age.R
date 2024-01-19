#' Handles Age Values to Code
#' 
#' This function returns a matching code value for a Age for the api to use to get data from State Cancer Profiles
#'
#' @param sex Either "under 19 years", "18 to 64 years", "21 to 64 years", "40 to 64 years", "50 to 64 years", "under 65 years"
#' 
#' @returns A string for its respective age
#' 
#' \dontrun{
#' @examples 
#' handle_age("under 19 years")
#' handle_age(age)
#' }
handle_age <- function(age) {
  age <- tolower(age)
  
  age_mapping <- c(
    "under 19 years" = "175",
    "18 to 64 years" = "174",
    "21 to 64 years" = "176",
    "40 to 64 years" = "122",
    "50 to 64 years" = "141",
    "under 65 years" = "006",
  )
  
  age_code <- age_mapping[age]
  
  if (is.null(age_code)) {
    stop("Invalid input")
  }
  
  return(as.character(age_code))
}
