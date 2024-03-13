#' Access to Income Data
#'
#' This function returns a data frame about income demographics from
#' State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either `"county"` or `"state"`.
#' @param income Either `"median family income"` or `"median household income"`.
#' @param race One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White (includes Hispanic)"`
#' - `"White non-Hispanic"`
#' - `"Black"`
#' - `"Amer. Indian/Alaskan Native (includes Hispanic)"`
#' - `"Asian or Pacific Islander (includes Hispanic)"`
#' - `"Hispanic (Any Race)`.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom dplyr mutate across
#' @importFrom stats setNames
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Dollars, Rank.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_income(
#'   area = "wa",
#'   areatype = "county",
#'   income = "median household income",
#'   race = "all races (includes hispanic)"
#' )
#'
#' demo_income(
#'   area = "usa",
#'   areatype = "state",
#'   income = "median family income",
#'   race = "all races (includes hispanic)"
#' )
#'
#' demo_income(
#'   area = "pr",
#'   areatype = "county",
#'   income = "median family income",
#'   race = "all races (includes hispanic)"
#' )
#' }
demo_income <- function(area, areatype, income, race) {
  req <- create_request("demographics")

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "inc",
      demo = handle_income(income),
      race = handle_race(race),
      type = "manyareacensus",
      sortVariableName = "value",
      sortOrder = "default",
      output = 1
    ) %>%
    req_perform()

  resp <- process_resp(resp, "demographics") %>%
    mutate(Value..Dollars. = as.integer(Value..Dollars.))

  areatype_map <- c("county" = "County", "state" = "State")
  areatype_title <- areatype_map[areatype]

  resp %>%
    setNames(c(areatype_title, "FIPS", "Dollars", "Rank")) %>% 
    mutate(across(c("Dollars"), \(x) as.numeric(x)))
}
