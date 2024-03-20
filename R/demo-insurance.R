#' Access to Insurance Data
#'
#' This function returns a data frame about insurance demographics
#' from State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param insurance One of the following values:
#' - `"% Insured in demographic group, all income levels"`
#' - `"% Insured in demographic group, people at or below 138% of Poverty"`
#' - `"% Insured in demographic group, people at or below 200% of Poverty"`
#' - `"% Insured in demographic group, people at or below 250% of Poverty"`
#' - `"% Insured in demographic group, people at or below 400% of Poverty"`
#' - `"% Insured in demographic group, people between 138% - 400% of poverty"`
#' - `"% uninsured in demographic group, all income levels"`
#' - `"% uninsured in demographic group, people at or below 138% of Poverty"`
#' - `"% uninsured in demographic group, people at or below 200% of Poverty"`
#' - `"% uninsured in demographic group, people at or below 250% of Poverty"`
#' - `"% uninsured in demographic group, people at or below 400% of Poverty"`
#' - `"% uninsured in demographic group, people between 138% - 400% of poverty"`.
#' @template param-sex
#' @param age If you specified `"both sexes"` for `sex`
#' choose one of the following values:
#' - `"under 19 years"`
#' - `"18 to 64 years"`
#' - `"21 to 64 years"`
#' - `"40 to 64 years"`
#' - `"50 to 64 years"`
#' - `"under 65 years"`.
#'
#' Otherwise if you specified `"male"` or `"female"` for `sex`,
#' choose one of the following values:
#' - `"18 to 64 years"`
#' - `"40 to 64 years"`
#' - `"50 to 64 years"`
#' - `"Under 65 years"`.
#' @param race Only specify `race` if you specified `"state"` for `areatype`
#' - `"All Races (includes Hispanic)"`
#' - `"White (non-Hispanic)"`
#' - `"black (non-Hispanic)"`
#' - `"American Indian / Alaska Native (non-Hispanic)"`
#' - `"Asian (non-Hispanic)"`
#' - `"Hispanic (Any Race)"`.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate across
#' @importFrom stats setNames
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Percent, People, Rank.
#'
#' @family demographics
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_insurance(
#'   area = "usa",
#'   areatype = "state",
#'   insurance = "% Insured in demographic group, all income levels",
#'   sex = "both sexes",
#'   age = "18 to 64 years",
#'   race = "white (non-hispanic)"
#' )
#'
#' demo_insurance(
#'   area = "wa",
#'   areatype = "hsa",
#'   insurance = "% Insured in demographic group, all income levels",
#'   sex = "males",
#'   age = "18 to 64 years"
#' )
#'
#' demo_insurance(
#'   area = "dc",
#'   areatype = "county",
#'   insurance = "% Insured in demographic group, all income levels",
#'   sex = "males",
#'   age = "18 to 64 years"
#' )
#' }
demo_insurance <- function(area, areatype, insurance, sex, age, race = NULL) {
  not_all_races <- c(
    "white non hispanic", "black non hispanic", "american indian / alaska native non-hispanic",
    "asian non-hispanic", "hispanic (any race)"
  )

  req <- create_request("demographics")

  if ((sex == "males" || sex == "females") && (age == "under 19 years" || age == "21 to 64 years")) {
    cli_abort("For males and females, age CANNOT be under 19 years OR 21 to 64 years")
  } else if (areatype == "state" && is.null(race)) {
    cli_abort("For areatype State, Race must not be null")
  } else if ((areatype == "state" && race %in% not_all_races) && (age == "under 19 years" || age == "21 to 64 years")) {
    cli_abort("For state data, only all races can have values under 19 years OR 21 to 64 years")
  }

  if ((areatype == "county" || areatype == "hsa") && !is.null(race)) {
    cli_abort("For areatype County and HSA, Race must be NULL.")
  }

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "ins",
      demo = handle_insurance(insurance),
      race = handle_race(race),
      sex = handle_sex(sex),
      age = handle_age(age),
      type = "manyareacensus",
      sortVariableName = "value",
      sortOrder = "default",
      output = 1
    )

  if (!is.null(race)) {
    resp <- resp %>%
      req_url_query(race = handle_race(race))
  }

  resp <- resp %>%
    req_perform()

  resp <- process_resp(resp, "demographics")

  resp %>%
    setNames(c(get_area(areatype), "Percent", "People", "Rank")) %>%
    mutate(across(c("Percent", "People"), \(x) as.numeric(x)))
}
