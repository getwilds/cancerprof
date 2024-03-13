#' Access to Vaccines Data
#'
#' This function returns a data frame about vaccines risks
#' from State Cancer Profiles.
#'
#' @param vaccine One of the following values:
#' - `"percent with up to date hpv vaccination coverage, ages 13-15",`
#' - `"percent with up to date hpv vaccination coverage, ages 13-17"`.
#' @param sex One of the following values:
#' - `"both sexes"`
#' - `"male"`
#' - `"female"`.
#' 
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Percent,
#' Lower 95% CI, Upper 95% CI, Number of Respondents.
#' 
#' @family risks
#'
#' @export
#'
#' @examples
#' \dontrun{
#' risk_vaccines(
#'   vaccine = "percent with up to date hpv vaccination coverage, ages 13-15",
#'   sex = "both sexes"
#' )
#'
#' risk_vaccines(
#'   vaccine = "percent with up to date hpv vaccination coverage, ages 13-17",
#'   sex = "females"
#' )
#' }
risk_vaccines <- function(vaccine, sex) {
  req <- create_request("risk")

  resp <- req %>%
    req_url_query(
      topic = "vaccine",
      risk = handle_vaccine(vaccine),
      sex = handle_sex(sex),
      type = "risk",
      sortVariableName = "percent",
      sortOrder = "default",
      output = 1
    )

  resp <- resp %>%
    req_perform()

  resp <- process_resp(resp, "risks")


  vaccine_type1 <- c(
    "percent who received 2+ doses of HPV vaccine, ages 13-15",
    "percent who received 3+ doses of HPV vaccine, ages 13-15"
  )

  if (vaccine %in% vaccine_type1) {
    resp %>%
      setNames(c(
        "State",
        "FIPS",
        "Met_Objective_of_80.0%?",
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI",
        "Number_of_Respondents"
      )) %>% 
      mutate(across(c(
        "Met_Objective_of_80.0%?",
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI",
        "Number_of_Respondents"
      ), \(x) as.numeric(x)))
  } else {
    resp %>%
      setNames(c(
        "State",
        "FIPS",
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI",
        "Number_of_Respondents"
      )) %>% 
      mutate(across(c(
        "Percent",
        "Lower_95%_CI",
        "Upper_95%_CI",
        "Number_of_Respondents"
      ), \(x) as.numeric(x)))
  }
}
