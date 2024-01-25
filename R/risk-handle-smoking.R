#' Handles Smoking Values to Code
#' 
#' This function returns a matching code value for a Smoking for the api to use to get data from State Cancer Profiles
#'
#' @param smoking Either "mammogram in past 2 years, ages 50-74", "mammogram in past 2 years, ages 40+",
#'                       "pap smear in past 3 years, no hysterectomy, ages 21-65",
#'                       "pap smear in past 3 years, no hysterectomy, ages 18+"
#' 
#' @returns A string for its respective Smoking Value
#' 
#' @examples
#' \dontrun{
#' handle_smoking("former smoker, quit 1 year+; ages 18+")
#' }
handle_smoking <- function(smoking) {
  smoking <- tolower(smoking)
  
  smoking_mapping <- c(
    "smokers (currnet); ages 18+" = "v19",
    
    "smokers (ever); ages 18+" = "v28",
    "e-cigarette use; ages 18+" = "v37",
    
    "former smoker; ages 18+" = "v300",
    "former smoker, quit 1 year+; ages 18+" = "v301",
    
    "smoking not allowed at work (current smokers)" = "v32",
    "smoking not allowed at work (former/never smokers)" = "v33",
    "smoking not allowed in home (current smokers)" = "v35",
    "smoking not allowed in home (former/never smokers)" = "v36",
    
    "smokers (stopped for 1 day or longer)" = "v30",
    "smoking not allowed at work (all people)" = "v31",
    "smoking not allowed in home (all people)" = "v34",
    
    "smoking laws (any)" = "v44",
    "smoking laws (bars)" = "v42",
    "smoking laws (restaurants)" = "v41",
    "smoking laws (workplace)" = "v40",
    "smoking laws (workplace; restaurant; & bar)" = "43"
  )
  
  smoking_code <- smoking_mapping[smoking]
  
  if (is.null(smoking_code)) {
    stop("Invalid input")
  }
  
  return(as.character(smoking_code))
}
