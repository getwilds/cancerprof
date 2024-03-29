#' Access to Poverty Data
#'
#' This function returns a data frame about poverty demographics
#' from State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param poverty One of the following values:
#' - `"families below poverty"`
#' - `"persistent poverty"`
#' - `"persons below poverty"`
#' - `"persons < 150% of poverty"`.
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
#' @importFrom cli cli_abort
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Percent, Households, Rank.
#'
#' @family demographics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_poverty(
#'   area = "WA",
#'   areatype = "county",
#'   poverty = "persistent poverty"
#' )
#'
#' demo_poverty(
#'   area = "usa",
#'   areatype = "state",
#'   poverty = "families below poverty",
#'   race = "black"
#' )
#'
#' demo_poverty(
#'   area = "dc",
#'   areatype = "hsa",
#'   poverty = "families below poverty",
#'   race = "All Races (includes Hispanic)"
#' )
#' }
demo_poverty <- function(area, areatype, poverty, race = NULL, sex = NULL) {
  req <- create_request("demographics")

  if (poverty == "persistent poverty" && (areatype == "hsa" || areatype == "state")) {
    cli_abort("For persistent poverty, areatype must be county")
  }
  if ((poverty == "persistent poverty" || poverty == "persons < 150% of poverty") &&
        (!is.null(race) || !is.null(sex))) {
    cli_abort("for persistent poverty and persons < 150% of poverty, Race and Sex must be NULL")
  } else if ((poverty == "families below poverty") && (!is.null(sex) || is.null(race))) {
    cli_abort("for families below poverty, Sex must be NULL and Race must not be NULL")
  } else if ((poverty == "persons below poverty") && (is.null(sex) || is.null(race))) {
    cli_abort("for persons below poverty, Sex and Race must not be NULL")
  }

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "pov",
      demo = handle_poverty(poverty),
      type = "manyareacensus",
      sortVariableName = "value",
      sortOrder = "default",
      output = 1
    )

  if (!is.null(race)) {
    resp <- resp %>%
      req_url_query(race = handle_race(race))
  }

  if (!is.null(sex)) {
    resp <- resp %>%
      req_url_query(sex = handle_sex(sex))
  }

  resp <- resp %>%
    req_perform()

  resp <- process_resp(resp, "demographics")

  if (poverty == "persistent poverty") {
    resp %>%
      setNames(c(
        get_area(areatype),
        "Persistent Poverty"
      ))
  } else {
    resp %>%
      setNames(c(
        get_area(areatype),
        "Percent",
        "People",
        "Rank"
      )) %>%
      mutate(across(c("Percent", "People"), \(x) as.numeric(x)))
  }
}
