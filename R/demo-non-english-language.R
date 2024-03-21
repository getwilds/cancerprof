#' Access to Non-English Language
#'
#' This function returns a data frame about language demographics
#' from State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param language The only permissible value is
#' `"language isolation"`.
#'
#' @importFrom httr2 req_url_query req_perform
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
#' demo_language(
#'   area = "WA",
#'   areatype = "county",
#'   language = "language isolation"
#' )
#'
#' demo_language(
#'   area = "dc",
#'   areatype = "hsa",
#'   language = "language isolation"
#' )
#'
#' demo_language(
#'   area = "usa",
#'   areatype = "state",
#'   language = "language isolation"
#' )
demo_language <- function(area, areatype, language) {
  req <- create_request("demographics")

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "lang",
      demo = handle_non_english(language),
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
      "Households",
      "Rank"
    )) %>%
    mutate(across(c("Percent", "Households"), \(x) as.numeric(x)))
}
