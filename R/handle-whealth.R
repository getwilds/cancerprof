#' Handles Women's Health Values to Code
#' 
#' This function returns a matching code value for a Women's Health for the api to use to get data from State Cancer Profiles
#'
#' @param whealth Either "mammogram in past 2 years, ages 50-74", "mammogram in past 2 years, ages 40+",
#'                       "pap smear in past 3 years, no hysterectomy, ages 21-65",
#'                       "pap smear in past 3 years, no hysterectomy, ages 18+"
#'                       
#' @importFrom rlang is_na
#'                       
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective Women's Health Value
#' 
#' @examples
#' \dontrun{
#' handle_whealth("mammogram in past 2 years, ages 50-74")
#' }
handle_whealth <- function(whealth) {
  whealth <- tolower(whealth)
  
  whealth_mapping <- c(
    "mammogram in past 2 years, ages 50-74" = "v05",
    "mammogram in past 2 years, ages 40+" = "v06",
    "pap smear in past 3 years, no hysterectomy, ages 21-65" = "v17",
    "pap smear in past 3 years, no hysteroetomy, ages 21-65" = "v17"
    
    #removed from scp
    # "pap smear in past 3 years, no hysterectomy, ages 18+" = "v11"
  )
  
  whealth_code <- whealth_mapping[whealth]
  
  if (is_na(whealth_code)) {
    stop("Invalid womens health input, please check the documentation for valid inputs")
  }
  
  return(as.character(whealth_code))
}


handle_whealth("pap smear in past 3 years, no hysteroetomy, ages 21-65")
