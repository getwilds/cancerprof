#' Handles Vaccines Values to Code
#' 
#' This function returns a matching code value for a Vaccines for the api to use to get data from State Cancer Profiles
#'
#' @param vaccine Either "mammogram in past 2 years, ages 50-74", "mammogram in past 2 years, ages 40+",
#'                       "pap smear in past 3 years, no hysterectomy, ages 21-65",
#'                       "pap smear in past 3 years, no hysterectomy, ages 18+"
#' 
#' @returns A string for its respective Vaccine Value
#' 
#' @examples
#' \dontrun{
#' handle_vaccine("percent who received 2+ doses of HPV vaccine, ages 13-17")
#' }
handle_vaccine <- function(vaccine) {
  vaccine <- tolower(vaccine)
  
  vaccine_mapping <- c(
    "percent who received 2+ doses of hpv vaccine, ages 13-15" = "v90",
    "percent who received 2+ doses of hpv vaccine, ages 13-17" = "v91",
    "percent who received 3+ doses of hpv vaccine, ages 13-15" = "v70",
    "percent who received 3+ doses of hpv vaccine, ages 13-17" = "v71"
  )
  
  vaccine_code <- vaccine_mapping[vaccine]
  
  if (is.null(vaccine_code)) {
    stop("Invalid input")
  }
  
  return(as.character(vaccine_code))
}

handle_vaccine("percent who received 3+ doses of hpv vaccine, ages 13-17")

