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
#' @returns A string for its respective Colorectal Screening Value
#' 
#' @examples 
#' \dontrun{
#' handle_screening("guidance sufficient crc, ages 50-75")
#' }
handle_screening <- function(screening) {
  screening <- tolower(screening)
  
  screening_mapping <- c(
    "ever had colorectal endoscopy (sigmoidoscopy or colonoscopy), ages 50+" = "v09",
    "ever had fobt, ages 50-75" = "v304",
    "fobt (1yr) / flex sig (5yr) / fobt (3yr) / colonoscopy (10yr), ages 50-75" = "v14",
    "guidance sufficient crc, ages 50-75" = "v303",
    "had colonoscopy in past 10 years, ages 50-75" = "v302",
    "home-based fecal occult blood test (fobt) in past two years" = "v13",
    "home-based fobt in the past two years or ever had a colorectal endoscopy" = "v59"
  )
  
  screening_code <- screening_mapping[screening]
  
  if (is.null(screening_code)) {
    stop("Invalid input")
  }
  
  return(as.character(screening_code))
}
