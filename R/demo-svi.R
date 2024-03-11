#' Access to Social Vulnerability Index (SVI) Data
#'
#' This function returns a data frame about social vulnerability index (SVI)
#' from State Cancer Profiles.
#'
#' @param area A state/territory abbreviation or USA.
#' @param svi One of the following values:
#' - `"Overall"`
#' - `"socioeconomic status"`
#' - `"household characteristics"`
#' - `"racial & ethinic minority status"`
#' - `"housing type & transportation"`.
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#'
#' @returns A data frame with the following columns: County, FIPS, Score.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_svi(
#'   area = "WA",
#'   svi = "overall"
#' )
#'
#' demo_svi(
#'   area = "usa",
#'   svi = "overall"
#' )
#'
#' demo_svi(
#'   area = "dc",
#'   svi = "socioeconomic status"
#' )
#' }
demo_svi <- function(area, svi) {
  req <- create_request("demographics")

  resp <- req %>%
    req_url_query(
      stateFIPS = fips_scp(area),
      areatype = "county",
      topic = "svi",
      demo = handle_svi(svi),
      type = "manyareacensus",
      sortVariableName = "value",
      sortOrder = "default",
      output = 1
    ) %>%
    req_perform()


  resp <- process_resp(resp, "demographics")

  resp %>%
    setNames(c("County", "FIPS", "Score"))
}
