#' Handles Cancer Stage Values to Code
#' 
#' This function returns a matching code value for a Cancer Stage for the api to use to get data from State Cancer Profiles
#'
#' @param education Either "less than 9th grade", "at least high school", "at least bachelors degree"
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
  
  if (is.null(stage_code)) {
    stop("Invalid input")
  }
  
  return(as.character(stage_code))
}
