#' Access to Insurance Data
#'
#' This function returns a data frame from Insurance in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param insurance Either
#'                  "% Insured in demographic group, all income levels", "% Insured in demographic group, people at or below 138% of Poverty"
#'                  "% Insured in demographic group, people at or below 200% of Poverty", "% Insured in demographic group, people at or below 250% of Poverty"
#'                  "% Insured in demographic group, people at or below 400% of Poverty","% Insured in demographic group, people between 138% - 400% of poverty"
#'                  "% uninsured in demographic group, all income levels","% uninsured in demographic group, people at or below 138% of Poverty"
#'                  "% uninsured in demographic group, people at or below 200% of Poverty","% uninsured in demographic group, people at or below 250% of Poverty"
#'                  "% uninsured in demographic group, people at or below 400% of Poverty","% uninsured in demographic group, people between 138% - 400% of poverty"
#'@param sex Either "both sexes", "male", "female"
#'@param age Either "under 19 years", "18 to 64 years","21 to 64 years","40 to 64 years","50 to 64 years","under 65 years" for "both sexes"
#'                  "18 to 64 years","40 to 64 years","50 to 64 years","Under 65 years" for "males" and "females"
#'                  
#' @importFrom httr2 req_url_query req_perform
#' @importFrom cli cli_abort
#'
#' @returns A data frame with the following columns "County", "FIPS", "Percent", "People", "Rank"
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' demo_insurance("usa", "state", "% Insured in demographic group, all income levels", "both sexes", "under 19 years")
#' demo_insurance("wa", "hsa", "% Insured in demographic group, all income levels", "males", "18 to 64 years")
#' demo_insurance("dc", "county", "% Insured in demographic group, all income levels", "males", "18 to 64 years")
#' }
demo_insurance <- function(area, areatype, insurance, sex, age) {
  
  req <- create_request("demographics")
  
  resp <- req %>%
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      topic="ins",
      demo=handle_insurance(insurance),
      race="00",
      sex=handle_sex(sex),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    )

  if ((sex == "males" || sex == "females") & (age == "under 19 years" || age =="21 to 64 years")) {
    cli_abort("For males and females, age CANNOT be uner 19 years OR 21 to 64 years")
  } 
  
  resp <- resp %>% 
    req_url_query(
      age=handle_age(age)
    ) %>%
  req_perform()

  resp <- process_response(resp)
  
  areatype_map <- c("county" = "County", "hsa" = "Health Service Area", "state" = "State")
  areatype_title <- areatype_map[areatype]
  
  resp %>% 
    setNames(c(areatype_title, "FIPS", "Percent", "People", "Rank"))
}
