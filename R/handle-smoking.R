#' Handles Smoking Values to Code
#'
#' This function returns a matching code value for smoking
#' for the api to use to get data from State Cancer Profiles.
#'
#' @param smoking The only permissible values are
#' - `"smoking laws (any)"`
#' - `"smoking laws (bars)"`
#' - `"smoking laws (restaurants)"`
#' - `"smoking laws (workplace)"`
#' - `"smoking laws (workplace; restaurant; & bar)"`
#' - `"smokers (stopped for 1 day or longer)"`
#' - `"smoking not allowed at work (all people)"`
#' - `"smoking not allowed in home (all people)"`
#' - `"smoking not allowed at work (current smokers)"`
#' - `"smoking not allowed at work (former/never smokers)"`
#' - `"smoking not allowed in home (current smokers)"`
#' - `"smoking not allowed in home (former/never smokers)"`
#' - `"former smoker; ages 18+"`
#' - `"former smoker, quit 1 year+; ages 18+"`
#' - `"smokers (ever); ages 18+"`
#' - `"e-cigarette use; ages 18+"`
#' - `"smokers (current); ages 18+"`.
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective Smoking Value
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' handle_smoking("former smoker, quit 1 year+; ages 18+")
#' }
handle_smoking <- function(smoking) {
  smoking <- tolower(smoking)

  smoking_mapping <- c(
    "smokers (current); ages 18+" = "v19",
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

  if (is_na(smoking_code)) {
    stop(
      paste(
        "Invalid smoking input, please check",
        "the documentation for valid inputs"
      )
    )
  }

  return(as.character(smoking_code))
}
