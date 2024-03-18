#' Access to Food Insecurity Data
#'
#' This function returns a data frame about food demographics from
#' State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either `"county"` or `"state"`.
#' @param food One of the following values:
#' - `"food insecurity"`
#' - `"limited access to healthy food"`.
#' @param race One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White non-Hispanic"`
#' - `"Black (includes Hispanic)"`
#' - `"Hispanic (Any Race)`.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate across
#' @importFrom stats setNames
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Value, People.
#'
#' @family demographics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_food(
#'   area = "wa",
#'   areatype = "county",
#'   food = "food insecurity",
#'   race = "black"
#' )
#'
#' demo_food(
#'   area = "usa",
#'   areatype = "state",
#'   food = "limited access to healthy food"
#' )
#'
#' demo_food(
#'   area = "pr",
#'   areatype = "county",
#'   food = "food insecurity",
#'   race = "all races (includes hispanic)"
#' )
#' }
demo_food <- function(area, areatype, food, race = NULL) {
  req <- create_request("demographics")

  if (food == "limited access to healthy food" && !is.null(race)) {
    cli_abort("For limited access to healthy food, Race must be NULL.")
  } else if (food == "food insecurity" && is.null(race)) {
    cli_abort("For food insecurity, Race must NOT be NULL.")
  }

  req_draft <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "food",
      demo = handle_food(food),
      type = "manyareacensus",
      sortVariableName = "value",
      sortOrder = "default",
      output = 1
    )

  if (!is.null(race)) {
    req_draft <- req_draft %>%
      req_url_query(race = handle_race(race))
  }

  resp <- req_draft %>%
    req_perform()

  resp <- process_resp(resp, "demographics")

  if (food == "limited access to healthy food") {
    resp %>%
      setNames(c(get_area(areatype), "Percent", "People")) %>%
      mutate(across(c("Percent", "People"), \(x) as.numeric(x)))
  } else if (food == "food insecurity") {
    resp %>%
      setNames(c(get_area(areatype), "Percent")) %>%
      mutate(across(c("Percent"), \(x) as.numeric(x)))
  }
}
