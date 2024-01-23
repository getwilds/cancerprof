#' Handles Food Values to Code
#' 
#' This function returns a matching code value for a Food for the api to use to get data from State Cancer Profiles
#'
#' @param sex Either "food insecurity" or "limited access to healthy food"
#' 
#' @returns A string for its respective food
#' 
#' \dontrun{
#' @examples 
#' handle_food("limited access to healthy food")
#' }
handle_food <- function(food) {
  food <- tolower(food)
  
  food_mapping <- c(
    "food insecurity" = "03003",
    "limited access to healthy food" = "03004"
  )
  
  food_code <- food_mapping[food]
  
  if (is.null(food_code)) {
    stop("Invalid input")
  }
  
  return(as.character(food_code))
}
