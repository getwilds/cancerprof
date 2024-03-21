#' Access to Mobility Data
#'
#' This function returns a data frame about mobility demographics
#' from State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @template param-areatype
#' @param mobility The only permissible values are
#' - `"i haven't moved (in past year)"`
#' - `"moved from outside us (in past year)"`
#' - `"moved, different state (in past year)"`
#' - `"moved, different county, same state (in past year)"`
#' - `"moved, same county (in past year)"`.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
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
#' demo_mobility(
#'   area = "WA",
#'   areatype = "county",
#'   mobility = "moved, different county, same state (in past year)"
#' )
#'
#' demo_mobility(
#'   area = "usa",
#'   areatype = "state",
#'   mobility = "moved, same county (in past year)"
#' )
#'
#' demo_mobility(
#'   area = "dc",
#'   areatype = "hsa",
#'   mobility = "moved, same county (in past year)"
#' )
#' }
demo_mobility <- function(area, areatype, mobility) {
  req <- create_request("demographics")

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = tolower(areatype),
      topic = "mob",
      demo = handle_mobility(mobility),
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
      "People",
      "Rank"
    )) %>%
    mutate(across(c("Percent", "People"), \(x) as.numeric(x)))
}
