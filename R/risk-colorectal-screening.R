#' Access to Colorectal Screening Data
#'
#' This function returns a data frame about
#' colorectal screening from State Cancer Profiles.
#'
#' @param screening One of the following values:
#' - `"ever had fobt, ages 50-75"`
#' - `"guidance sufficient crc, ages 50-75"`
#' - `"had colonoscopy in past 10 years, ages 50-75"`
#' - `"home blood stool test in the past year, ages 45-75"`
#' - `"received at least one recommended crc test, ages 45-75"`.
#' @param race One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White (non-Hispanic)"`
#' - `"Black (non-Hispanic)"`
#' - `"American Indian / Alaska Native (non-Hispanic)"`
#' - `"Asian / Pacific Islander (non-Hispanic)"`
#' - `"Hispanic (Any Race)"`.
#' @template param-sex
#' @param area A state/territory abbreviation or USA.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Percent, People Unemployed, Rank.
#'
#' @family risks
#'
#' @export
#'
#' @examples
#' \dontrun{
#' risk_colorectal_screening(
#'   screening = "home blood stool test in the past year, ages 45-75",
#'   race = "all races (includes hispanic)",
#'   sex = "both sexes"
#' )
#'
#' risk_colorectal_screening(
#'   screening = "ever had fobt, ages 50-75",
#'   area = "usa"
#' )
#' risk_colorectal_screening(
#'   screening = "received at least one recommended crc test, ages 45-75",
#'   race = "all races (includes hispanic)",
#'   sex = "both sexes"
#' )
#' }
risk_colorectal_screening <- function(screening, race = NULL, sex = NULL, area = NULL) {
  req <- create_request("risk")

  screening_type_1 <- c(
    "home blood stool test in the past year, ages 45-75",
    "received at least one recommended crc test, ages 45-75"
  )

  screening_type_2 <- c(
    "ever had fobt, ages 50-75",
    "guidance sufficient crc, ages 50-75",
    "had colonoscopy in past 10 years, ages 50-75"
  )

  if (screening %in% screening_type_1 && ((is.null(race) || is.null(sex)) || !is.null(area))) {
    cli_abort("For this screening type, Race and Sex must not be NULL, and Area must be NULL")
  } else if (screening %in% screening_type_2 && (is.null(area) || (!is.null(race) || !is.null(sex)))) {
    cli_abort("for this screening type, area must NOT be NULL and Race and Sex must be NULL")
  }

  resp <- req %>%
    req_url_query(
      topic = "colorec",
      risk = handle_screening(screening),
      type = "risk",
      sortVariableName = "percent",
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

  if (!is.null(area)) {
    resp <- resp %>%
      req_url_query(stateFIPS = fips_scp(area))
  }

  resp <- resp %>%
    req_perform()
  
  resp_url <- resp$url

  resp <- process_resp(resp, "risks")

  if (screening %in% screening_type_1) {
    resp$data <- resp$data %>%
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
  } else if (screening %in% screening_type_2) {
    resp$data <- resp$data %>%
      setNames(c(
        "County",
        "FIPS",
        "Model_Based_Percent (95%_Confidence_Interval)",
        "Lower_95%_CI",
        "Upper_95%_CI"
      )) %>%
      mutate(across(c(
        "Model_Based_Percent (95%_Confidence_Interval)",
        "Lower_95%_CI",
        "Upper_95%_CI"
      ), \(x) as.numeric(x)))
  }
  
  process_metadata(resp, "risks", resp_url)
}
