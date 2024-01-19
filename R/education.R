#' Access to Education Data
#'
#' This function returns a data frame from Education in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county" or "HSA" (Health service area)
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' @param sex Either "both sexes", "male", "female"
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#'
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "Households", "Rank"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' demo_education("wa", "county", "at least high school", "males")
#' demo_education("usa", "state", "at least bachelors degree", "both sexes", "all races (includes hispanic)")
#' }
demo_education <- function(area, areatype, education, sex=NULL, race=NULL) {

  req <- create_request("demographics")

  if(education == "less than 9th grade" && (!is.null(race) || !is.null(sex))) {
    cli_abort("For Less than 9th Grade, Race and Sex must be NULL.")
  } else if (education == "at least high school" && !is.null(race) && is.null(sex)) {
    cli_abort("For At Least High School, Race must be NULL and Sex must be NOT NULL.")
  } else if (education == "at least bachelors degree" && (is.null(race) || is.null(sex))) {
    cli_abort("For At Least Bachelors Degree, Race and Sex must be NOT NULL.")
  }

  resp <- req %>%
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="ed",
      demo=handle_education(education),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    )

    if(!is.null(race)) {
      resp <- resp %>%
        req_url_query(race=handle_race(race))
    }

    if(!is.null(sex)) {
      resp <- resp %>%
        req_url_query(sex=handle_sex(sex))
    }

    resp <- resp %>%
      req_perform()

    resp <- process_response(resp)
    
    if (areatype == "county") {
      resp %>% 
        setNames(c("County", "FIPS", "Percent", "Households", "Rank")) 
    } else if (areatype == "hsa") {
      resp %>% 
        setNames(c("Health Service Area", "FIPS", "Percent", "Households", "Rank"))
    } else if (areatype == "state") {
      resp %>% 
        setNames(c("State", "FIPS", "Percent", "Households", "Rank"))
    }
}
