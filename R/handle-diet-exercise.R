#' Handles Diet & Exercise Values to Code
#' 
#' This function returns a matching code value for a Diet & Exercise for the api to use to get data from State Cancer Profiles
#'
#' @param income Either "ever had colorectal endoscopy (sigmoidoscopy or colonoscopy), ages 50+",
#'               "ever had fobt, ages 50-75", "fobt (1yr) / flex sig (5yr) / fobt (3yr) / colonoscopy (10yr), ages 50-75",
#'               "guidance sufficient crc, ages 50-75", "had colonoscopy in past 10 years, ages 50-75",
#'               "home-based fecal occult blood test (fobt) in past two years",
#'               "home-based fobt in the past two years or ever had a colorectal endoscopy"
#' 
#' @returns A string for its respective income
#' 
#' \dontrun{
#' @examples 
#' handle_diet_exercise("guidance sufficient crc, ages 50-75")
#' }
handle_diet_exercise <- function(diet_exercise) {
  diet_exercise <- tolower(diet_exercise)
  
  diet_exercise_mapping <- c(
    "bmi is healthy, ages 20+" = "v01",
    "bmi is obese, ages 20+" = "v02",
    "bmi is obese, high school survey" = "v82",
    "bmi is overweight, high school survey" = "v83",
    "consumed 1 or more fruits per day" = "v50",
    "consumed 1 or more vegetables per day" = "v51",
    "no leisure time physical activity" = "v18"
  )
  
  diet_exercise_code <- diet_exercise_mapping[diet_exercise]
  
  if (is.null(diet_exercise_code)) {
    stop("Invalid input")
  }
  
  return(as.character(diet_exercise_code))
}
