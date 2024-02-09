#' Handles Vaccines Values to Code
#' 
#' This function returns a matching code value for a Vaccines for the api to use to get data from State Cancer Profiles
#'
#' @param vaccine Either "mammogram in past 2 years, ages 50-74", "mammogram in past 2 years, ages 40+",
#'                       "pap smear in past 3 years, no hysterectomy, ages 21-65",
#'                       "pap smear in past 3 years, no hysterectomy, ages 18+"
#' 
#' @importFrom rlang is_na
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
    # removed from scp
    # "percent who received 2+ doses of hpv vaccine, ages 13-15" = "v90",
    # "percent who received 2+ doses of hpv vaccine, ages 13-17" = "v91",
    # "percent who received 3+ doses of hpv vaccine, ages 13-15" = "v70",
    # "percent who received 3+ doses of hpv vaccine, ages 13-17" = "v71"
    
    "percent with up to date hpv vaccination coverage, ages 13-15" = "v281",
    "percent with up to date hpv vaccination coverage, ages 13-15" = "v282"
  )
  
  vaccine_code <- vaccine_mapping[vaccine]
  
  if (is_na(vaccine_code)) {
    stop("Invalid vaccine input, please check the documentation for valid inputs")
  }
  return(as.character(vaccine_code))
}
