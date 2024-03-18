#' Access to Alcohol Screening and Risk Data
#'
#' This function returns a data frame about alcohol risks
#' from State Cancer Profiles.
#'
#' @param alcohol The only permissible value is
#' `paste("binge drinking (4+ drinks on one occasion for women,",
#'        "5+ drinks for one occasion for men), ages 21+")
#' @param race One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White (non-Hispanic)"`
#' - `"Black (non-Hispanic)"`
#' - `"American Indian / Alaska Native (non-Hispanic)"`
#' - `"Asian / Pacific Islander (non-Hispanic)"`
#' - `"Hispanic (Any Race)"`.
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
#' Area Type, Area Code, Percent, Lower 95% CI,
#' Upper 95% CI, Number of Respondents.
#' 
#' @family risks
#'
#' @export
#'
#' @examples
#' \dontrun{
#' risk_alcohol(
#'   alcohol = paste(
#'     "binge drinking (4+ drinks on one occasion for women,",
#'     "5+ drinks for one occasion for men), ages 21+"
#'   ),
#'   race = "all races (includes hispanic)",
#'   sex = "both sexes"
#' )
#' }
risk_alcohol <- function(alcohol, race, sex) {
  req <- create_request("risk")

  resp <- req %>%
    req_url_query(
      topic = "alcohol",
      risk = handle_alcohol(alcohol),
      race = handle_race(race),
      sex = handle_sex(sex),
      type = "risk",
      sortVariableName = "percent",
      sortOrder = "default",
      output = 1
    ) %>%
    req_perform()

  resp <- process_resp(resp, "risks")

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
