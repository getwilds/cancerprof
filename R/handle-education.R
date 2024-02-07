#' Handles Education Values to Code
#' 
#' This function returns a matching code value for a Education for the api to use to get data from State Cancer Profiles
#'
#' @param education Either "less than 9th grade", "at least high school", "at least bachelors degree"
#' 
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective education
#' 
#' @examples 
#' \dontrun{
#' handle_education("at least bachelors degree")
#' }
handle_education <-function(education) {
  education <- tolower(education)
  
  edu_mapping <- c(
    "less than 9th grade" = "00004",
    "at least high school" = "00109",
    "at least bachelors degree" = "00006"
  )
  
  edu_code <- edu_mapping[education]
  
  if (is_na(edu_code)) {
    stop("Invalid education input, please check the documentation for valid inputs")
  }
  
  return(as.character(edu_code))
}

