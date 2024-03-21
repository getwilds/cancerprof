#' Access to Workforce Data
#'
#' This function returns a data frame from Workforce in State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param workforce The only permissible value is
#' `"unemployed"`
#' @param race One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White (includes Hispanic)"`
#' - `"White non-Hispanic"`
#' - `"Black"`
#' - `"Amer. Indian/Alaskan Native (includes Hispanic)"`
#' - `"Asian or Pacific Islander (includes Hispanic)"`
#' - `"Hispanic (Any Race)`.
#' @template param-sex
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Percent, People Unemployed, Rank.
#'
#' @family demographics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_workforce(
#'   area = "WA",
#'   areatype = "county",
#'   workforce = "unemployed",
#'   race = "all races (includes hispanic)",
#'   sex = "both sexes"
#' )
#'
#' demo_workforce(
#'   area = "usa",
#'   areatype = "state",
#'   workforce = "unemployed",
#'   race = "all races (includes hispanic)",
#'   sex = "females"
#' )
#'
#' demo_workforce(
#'   area = "pr",
#'   areatype = "hsa",
#'   workforce = "unemployed",
#'   race = "all races (includes hispanic)",
#'   sex = "both sexes"
#' )
#' }
demo_workforce <- function(area, areatype, workforce, race, sex) {
  req <- create_request("demographics")

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "work",
      demo = handle_workforce(workforce),
      race = handle_race(race),
      sex = handle_sex(sex),
      type = "manyareacensus",
      sortVariableName = "value",
      sortOrder = "default",
      output = 1
    ) %>%
    req_perform()

  resp <- process_resp(resp, "demographics")

  resp %>%
    setNames(c(
      get_area(areatype),
      "Percent",
      "People_Unemployed",
      "Rank"
    )) %>%
    mutate(across(c("Percent", "People_Unemployed"), \(x) as.numeric(x)))
}
