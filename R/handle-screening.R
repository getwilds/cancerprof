#' Handles Colorectal Screening Values to Code
#' 
#' This function returns a matching code value for a Colorectal Screening for the api to use to get data from State Cancer Profiles
#'
#' @param screening Either "ever had colorectal endoscopy (sigmoidoscopy or colonoscopy), ages 50+",
#'               "ever had fobt, ages 50-75", "fobt (1yr) / flex sig (5yr) / fobt (3yr) / colonoscopy (10yr), ages 50-75",
#'               "guidance sufficient crc, ages 50-75", "had colonoscopy in past 10 years, ages 50-75",
#'               "home-based fecal occult blood test (fobt) in past two years",
#'               "home-based fobt in the past two years or ever had a colorectal endoscopy"
#' 
#' @importFrom rlang is_na
#' 
#' @returns A string for its respective Colorectal Screening Value
#' 
#' @noRd
#' 
#' @examples 
#' \dontrun{
#' handle_screening("guidance sufficient crc, ages 50-75")
#' }
handle_screening <- function(screening) {
  screening <- tolower(screening)
  
  screening_mapping <- c(
    "ever had fobt, ages 50-75" = "v304",
    "guidance sufficient crc, ages 50-75" = "v303",
    "had colonoscopy in past 10 years, ages 50-75" = "v302",
    
    "home blood stool test in the past year, ages 45-75" = "v520",
    "received at least one recommended crc test, ages 45-75" = "v521"
    
    #removed from scp
    # "ever had colorectal endoscopy (sigmoidoscopy or colonoscopy), ages 50+" = "v09",
    # "fobt (1yr) / flex sig (5yr) / fobt (3yr) / colonoscopy (10yr), ages 50-75" = "v14",
    # "home-based fecal occult blood test (fobt) in past two years" = "v13",
    # "home-based fobt in the past two years or ever had a colorectal endoscopy" = "v59"
  )
  
  screening_code <- screening_mapping[screening]
  
  if (is_na(screening_code)) {
    stop("Invalid screening input, please check the documentation for valid inputs")
  }
  
  return(as.character(screening_code))
}
