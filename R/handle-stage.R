#' Handles Cancer Stage Values to Code
#' 
#' This function returns a matching code value for a Cancer Stage for the api to use to get data from State Cancer Profiles
#'
#' @param stage Either "all stages" or "late stage (regional & distant)"
#' 
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective Cancer Stage
#' 
#' @examples 
#' \dontrun{
#' handle_stage("all stages")
#' }
handle_stage <-function(stage) {
  stage <- tolower(stage)
  
  stage_mapping <- c(
    "all stages" = "999",
    "late stage (regional & distant)" = "211"
  )
  
  stage_code <- stage_mapping[stage]
  
  if (is_na(stage_code)) {
    stop("Invalid stage input, please check the documentation for valid inputs")
  }
  
  return(as.character(stage_code))
}
