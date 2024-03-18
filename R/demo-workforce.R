#' Access to Workforce Data
#'
#' This function returns a data frame from Workforce in State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype One of the following values:
#' - `"county"`
#' - `"hsa"` (Health Service Area)
#' - `"state"`.
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

  area_type <- get_area(areatype)[1]
  area_code <- get_area(areatype)[2]

  resp %>%
    setNames(c(
      area_type,
      area_code,
      "Percent",
      "People_Unemployed",
      "Rank"
    )) %>%
    mutate(across(c("Percent", "People_Unemployed"), \(x) as.numeric(x)))
}
