#' Handles Non-English Language Values to Code
#' 
#' This function returns a matching code value for Non-English Language for the api to use to get data from State Cancer Profiles
#'
#' @param language "language isolation"
#' 
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective language Value
#' 
#' @noRd
#' 
#' @examples
#' \dontrun{
#' handle_non_english("language isolation")
#' }
handle_non_english <- function(language) {
  language <- tolower(language)
  
  language_mapping <- c(
    "language isolation" = "00015"
  )
  
  language_code <- language_mapping[language]
  
  if (is_na(language_code)) {
    stop("Invalid language input, please check the documentation for valid inputs")
  }
  
  return(as.character(language_code))
}