#' Handles Area Type Values
#'
#' This function screens the areatype value to check for invalid inputs
#' and changes the case to lower.
#'
#' @template param-areatype
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective areatype
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_areatype("county")
#' }
handle_areatype <- function(areatype) {
  areatype <- tolower(areatype)
  
  valid_areatype <- c("county", "hsa", "state")
  
  if (!areatype %in% valid_areatype) {
    stop("Invalid areatype input, please check the documentation for valid inputs")
  }
  
  return(areatype)
}
