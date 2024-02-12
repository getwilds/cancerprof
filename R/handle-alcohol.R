#' Handles Women's Alcohol Values to Code
#' 
#' This function returns a matching code value for Alcohol for the api to use to get data from State Cancer Profiles
#'
#' @param alcohol binge drinking (4+ drinks on one occasion for women, 5+ drinks for one occasion for men), ages 21+
#' 
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective Women's Health Value
#' 
#' @examples
#' \dontrun{
#' handle_alcohol(paste("binge drinking (4+ drinks on one occasion for women,"
#'                       "5+ drinks for one occasion for men), ages 21+"))
#' }
handle_alcohol <- function(alcohol) {
  alcohol <- tolower(alcohol)
  
  alcohol_mapping <- c(
    "binge drinking (4+ drinks on one occasion for women, 5+ drinks for one occasion for men), ages 21+" = "v505"
  )
  
  alcohol_code <- alcohol_mapping[alcohol]
  
  if (is_na(alcohol_code)) {
    stop("Invalid alcohol input, please check the documentation for valid inputs")
  }
  
  return(as.character(alcohol_code))
}
