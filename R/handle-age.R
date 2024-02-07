#' Handles Age Values to Code
#' 
#' This function returns a matching code value for a Age for the api to use to get data from State Cancer Profiles
#'
#' @param age Either "under 19 years", "18 to 64 years", "21 to 64 years", "40 to 64 years", "50 to 64 years", "under 65 years"
#' 
#' @returns A string for its respective age
#' 
#' @examples 
#' \dontrun{
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
    
    #cancer ages
    "all ages" = "001",
    "ages <50" = "009",
    "ages 50+" = "136",
    "ages <65" = "006",
    "ages 65+" = "157",
    "ages <15" = "016",
    "ages <20" = "015"
  )
  
  age_code <- age_mapping[age]
  
  if (is_na(age_code)) {
    stop("Invalid age input, please check the documentation for valid inputs")
  }
  
  return(as.character(age_code))
}
