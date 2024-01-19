#' Handles Insurance Values to Code
#' 
#' This function returns a matching code value for a insurance value for the api to use to get data from State Cancer Profiles
#'
#' @param insurance Either "% insured in demographic group, all income levels" = "00030",
#'                  "% insured in demographic group, people at or below 138% of poverty",
#'                  "% insured in demographic group, people at or below 200% of poverty",
#'                  "% insured in demographic group, people at or below 250% of poverty",
#'                  "% insured in demographic group, people at or below 400% of poverty",
#'                  "% insured in demographic group, people between 138% - 400% of poverty",
#'                  "% uninsured in demographic group, all income levels",
#'                  "% uninsured in demographic group, people at or below 138% of poverty",
#'                  "% uninsured in demographic group, people at or below 200% of poverty", 
#'                  "% uninsured in demographic group, people at or below 250% of poverty",
#'                  "% uninsured in demographic group, people at or below 400% of poverty",
#'                  "% uninsured in demographic group, people between 138% - 400% of poverty"
#' 
#' @returns A string for its respective race
#' 
#' \dontrun{
#' @examples 
#' handle_insurance("% insured in demographic group, all income levels") ### WHY doesnt this work?
#' }

handle_insurance <- function(insurance) {

  insurance <- tolower(insurance)
  
  insurance_mapping <- c(
    "% insured in demographic group, all income levels" = "00030",
    "% insured in demographic group, people at or below 138% of poverty" = "00033",
    "% insured in demographic group, people at or below 200% of poverty" = "00031",
    "% insured in demographic group, people at or below 250% of poverty" = "00032",
    "% insured in demographic group, people at or below 400% of poverty" = "00034",
    "% insured in demographic group, people between 138% - 400% of poverty" = "00035",
    "% uninsured in demographic group, all income levels" = "00040",
    "% uninsured in demographic group, people at or below 138% of poverty" = "00043",
    "% uninsured in demographic group, people at or below 200% of poverty" = "00041",
    "% uninsured in demographic group, people at or below 250% of poverty" = "00042",
    "% uninsured in demographic group, people at or below 400% of poverty" = "00044",
    "% uninsured in demographic group, people between 138% - 400% of poverty" = "00045"
  )
  
  insurance_code <- insurance_mapping[insurance]
  
  if (is.null(insurance_code)) {
    stop("Invalid input")
  }
  
  return(as.character(insurance_code))
}


