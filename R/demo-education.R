#' Access to Education Data
#'
#' This function returns a data frame about education demographics from
#' State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param education One of the following values:
#' - `"less than 9th grade"`
#' - `"at least high school"`
#' - `"at least bachelors degree"`.
#' @template param-sex
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
#' @importFrom cli cli_abort
#' @importFrom stats setNames
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
#' demo_education(
#'   area = "wa",
#'   areatype = "county",
#'   education = "at least high school",
#'   sex = "males"
#' )
#'
#' demo_education(
#'   area = "usa",
#'   areatype = "state",
#'   education = "at least bachelors degree",
#'   sex = "both sexes",
#'   race = "all races (includes hispanic)"
#' )
#'
#' demo_education(
#'   area = "pr",
#'   areatype = "hsa",
#'   education = "less than 9th grade"
#' )
#' }
demo_education <- function(area, areatype, education, sex = NULL, race = NULL) {
  req <- create_request("demographics")

  if (education == "less than 9th grade" && (!is.null(race) || !is.null(sex))) {
    cli_abort("For Less than 9th Grade, Race and Sex must be NULL.")
  } else if (education == "at least high school" && (!is.null(race) || is.null(sex))) {
    cli_abort("For At Least High School, Race must be NULL and Sex must be NOT NULL.")
  } else if (education == "at least bachelors degree" && (is.null(race) || is.null(sex))) {
    cli_abort("For At Least Bachelors Degree, Race and Sex must be NOT NULL.")
  }

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "ed",
      demo = handle_education(education),
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
  
  resp$data <- resp$data %>%
    setNames(c(
      get_area(areatype),
      "Percent",
      "Households",
      "Rank"
    )) %>%
    mutate(across(c("Percent", "Households"), \(x) as.numeric(x)))
  
  process_metadata(resp)

}
