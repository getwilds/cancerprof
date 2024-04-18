#' Access to Cancer Mortality Data
#'
#' This function returns a data frame about cancer mortality
#' from State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param cancer One of the following values:
#' - `"all cancer sites"`
#' - `"bladder"`
#' - `"brain & ons"`
#' - `"breast (female)"`
#' - `"cervix"`
#' - `"childhood (ages <15, all sites)"`
#' - `"childhood (ages <20, all sites)"`
#' - `"colon & rectum"`
#' - `"esophagus"`
#' - `"kidney & renal pelvis"`
#' - `"leukemia"`
#' - `"liver & bile duct"`
#' - `"lung & bronchus"`
#' - `"melanoma of the skin"`
#' - `"non-hodgkin lymphoma"`
#' - `"oral cavity & pharynx"`
#' - `"ovary"`
#' - `"pancreas"`
#' - `"prostate"`
#' - `"stomach"`
#' - `"thyroid"`
#' - `"uterus (corpus & uterus, nos)"`
#' @param race One of the following values:
#' - `"All Races (includes Hispanic)"`
#' - `"White (non-Hispanic)"`
#' - `"Black (non-Hispanic)"`
#' - `"American Indian / Alaska Native (non-Hispanic)"`
#' - `"Asian / Pacific Islander (non-Hispanic)"`
#' - `"Hispanic (Any Race)"`.
#' @template param-sex
#' @param age One of the following values:
#' - `"all ages"`
#' - `"ages <50"`
#' - `"ages 50+"`
#' - `"ages <65"`
#' - `"ages 65+"`
#' - `"ages <15"`
#' - `"ages <20"`.
#' @param year One of the following values:
#' - `"latest 5 year average"`
#' - `"latest single year (us by state)"`.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#' @importFrom stats setNames
#' @importFrom dplyr mutate across all_of
#'
#' @returns A data frame with the following columns:
#' Area Type, Area Code, Met Healthy People Objective of ***?,
#' Age Adjusted Death Rate, Lower 95% CI Rate, Upper 95% CI Rate,
#' CI Rank, Lower CI Rank, Upper CI Rank, Annual Average Count,
#' Recent Trend, Recent 5 Year Trend, Lower 95% CI Trend, Upper 95% CI Trend.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mortality_cancer(
#'   area = "wa",
#'   areatype = "county",
#'   cancer = "all cancer sites",
#'   race = "black (non-hispanic)",
#'   sex = "both sexes",
#'   age = "ages 65+",
#'   year = "latest 5 year average"
#' )
#'
#' mortality_cancer(
#'   area = "usa",
#'   areatype = "state",
#'   cancer = "prostate",
#'   race = "all races (includes hispanic)",
#'   sex = "males",
#'   age = "ages 50+",
#'   year = "latest single year (us by state)"
#' )
#'
#' mortality_cancer(
#'   area = "wa",
#'   areatype = "hsa",
#'   cancer = "ovary",
#'   race = "all races (includes hispanic)",
#'   sex = "females",
#'   age = "ages 50+",
#'   year = "latest 5 year average"
#' )
#' }
mortality_cancer <- function(area, areatype, cancer, race, sex, age, year) {
  female_cancer <- c(
    "breast (female)",
    "ovary",
    "uterus (corpus & uterus, nos)"
  )

  childhood_cancer <- c(
    "childhood (ages <15, all sites)",
    "childhood (ages <20, all sites)"
  )

  if ((areatype == "county" || areatype == "hsa") && year == "latest single year (us by state)") {
    cli_abort("For year latest single year (us by state), areatype must be state")
  }

  if ((cancer %in% female_cancer) && (sex == "males" || sex == "both sexes")) {
    cli_abort("For this cancer type, sex must be females")
  } else if (cancer == "prostate" && (sex == "females" || sex == "both sexes")) {
    cli_abort("For prostate cancer, sex must be males.")
  }

  if (cancer == "childhood (ages <15, all sites)" && age != "ages <15") {
    cli_abort("For childhood (ages <15, all sites), age must be ages <15")
  } else if (cancer == "childhood (ages <20, all sites)" && age != "ages <20") {
    cli_abort("For childhood (ages <20, all sites), age must be ages <20")
  } else if ((!cancer %in% childhood_cancer) && (age == "ages <15" || age == "ages <20")) {
    cli_abort("For this cancer type, age cannot be ages <15 or ages <20")
  }

  req <- create_request("deathrates")

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      cancer = handle_cancer(cancer),
      race = handle_race(race),
      age = handle_age(age),
      year = handle_year(year),
      type = "death",
      sortVariableName = "rate",
      sortOrder = "default",
      output = 1
    )

  if (!is.null(sex)) {
    resp <- resp %>%
      req_url_query(sex = handle_sex(sex))
  }

  resp <- resp %>%
    req_perform()

  resp_url <- resp$url
  
  resp <- process_resp(resp, "mortality")

  names_to_numeric <- c(
    "Age_Adjusted_Death_Rate",
    "Lower_95%_CI_Rate",
    "Upper_95%_CI_Rate",
    "CI_Rank",
    "Lower_CI_Rank",
    "Upper_CI_Rank"
  )

  resp$data <- resp$data %>%
    setNames(c(
      get_area(areatype),
      "Met Healthy People Objective of ***?",
      "Age_Adjusted_Death_Rate",
      "Lower_95%_CI_Rate",
      "Upper_95%_CI_Rate",
      "CI_Rank",
      "Lower_CI_Rank",
      "Upper_CI_Rank",
      "Annual_Average_Count",
      "Recent_Trend",
      "Recent_5_Year_Trend",
      "Lower_95%_CI_Trend",
      "Upper_95%_CI_Trend"
    )) %>%
    mutate(across(c(
      all_of(names_to_numeric),
      "Recent_5_Year_Trend",
      "Lower_95%_CI_Trend",
      "Upper_95%_CI_Trend"
    ), \(x) as.numeric(x)))
  
  process_metadata(resp, "risks", resp_url)
}
