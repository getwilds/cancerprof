#' Handles Food Values to Code
#' 
#' This function returns a matching code value for a Food for the api to use to get data from State Cancer Profiles
#'
#' @param food Either "food insecurity" or "limited access to healthy food"
#' 
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective food
#' 
#' @examples 
#' \dontrun{
#' handle_food("limited access to healthy food")
#' }
handle_food <- function(food) {
  food <- tolower(food)
  
  food_mapping <- c(
    "food insecurity" = "03003",
    "limited access to healthy food" = "03004"
  )
  
  food_code <- food_mapping[food]
  
  if (is_na(food_code)) {
    stop("Invalid food input, please check the documentation for valid inputs")
  }
  
  return(as.character(food_code))
}
