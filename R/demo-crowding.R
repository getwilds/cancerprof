#' Access to Crowding Data
#'
#' This function returns a data frame about crowding demographics from State
#' Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param crowding The only permissible value is
#' `"household with >1 person per room"`.
#' @param race
#' One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White (includes Hispanic)"`
#' - `"White Non-Hispanic"`
#' - `"Black"`
#' - `"Amer. Indian/Alaskan Native (includes Hispanic)"`
#' - `"Asian or Pacific Islander (includes Hispanic)"`
#' - `"Hispanic (Any Race)"`.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
#' @importFrom tibble as_tibble
#'
#' @returns A data frame with the following columns: Area, Area Code,
#' Percent, Households, Rank.
#'
#' @family demographics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_crowding(
#'   area = "WA",
#'   areatype = "county",
#'   crowding = "household with >1 person per room",
#'   race = "All Races (includes Hispanic)"
#' )
#'
#' demo_crowding(
#'   area = "usa",
#'   areatype = "state",
#'   crowding = "household with >1 person per room",
#'   race = "All Races (includes Hispanic)"
#' )
#'
#' demo_crowding(
#'   area = "pr",
#'   areatype = "hsa",
#'   crowding = "household with >1 person per room",
#'   race = "black"
#' )
#' }
demo_crowding <- function(area, areatype, crowding, race) {
  req <- create_request("demographics")

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "crowd",
      demo = handle_crowding(crowding),
      race = handle_race(race),
      type = "manyareacensus",
      sortVariableName = "value",
      sortOrder = "default",
      output = 1
    ) %>%
    req_perform()

  resp_url <- resp$url
  
  resp <- process_resp(resp, "demographics")
  
  resp$data <- resp$data %>%
    setNames(c(
      get_area(areatype),
      "Percent",
      "Households",
      "Rank"
    )) %>%
    mutate(across(c("Percent", "Households"), \(x) as.numeric(x)))

  process_metadata(resp, resp_url)
}
