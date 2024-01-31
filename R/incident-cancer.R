#' Access to Cancer Incident Data
#' 
#' This function returns a data frame from Cancer Incident in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param cancer Either "all cancer sites","bladder", "brain & ons", "breast (female)", "breast (female in situ)", "cervix", 
#'                      "childhood (ages <15, all sites)", "childhood (ages <20, all sites)", "colon & rectum", "esophagus", 
#'                      "kidney & renal pelvis", "leukemia", "liver & bile duct", "lung & bronchus", "melanoma of the skin", 
#'                      "non-hodgkin lymphoma", "oral cavity & pharynx", "ovary", "pancreas", "prostate", "stomach", 
#'                      "thyroid", "uterus (corpus & uterus, nos)"
#' @param race One of the following values: "all races (includes hispanic)", "white (non-hispanic)", 
#'                                          "black (non-hispanic)", "amer. indian / ak native (non-hispanic)", 
#'                                          "asian / pacific islander (non-hispanic)","hispanic (any race)"
#' @param sex Either "both sexes", "males", "females"
#' @param age Either "all ages", "ages <50", "ages 50+", "ages <65", "ages 65+"
#' @param stage Either "all stages" or "late stage (regional & distant)"
#' 
#' @returns A data frame with the following columns "State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' incidence_cancer("wa", "county", "lung & bronchus", "all races (includes hispanic)", "males", "ages 50+", "late stage (regional & distant)", "latest 5 year average")
#' incidence_cancer("usa", "state", "lung & bronchus", "all races (includes hispanic)", "males", "ages 50+", "late stage (regional & distant)")
#' incidence_cancer("percent who received 2+ doses of HPV vaccine, ages 13-17", "both sexes")
#' incidence_cancer("percent who received 3+ doses of HPV vaccine, ages 13-17", "females")
#' }

incidence_cancer("wa", "county", "lung & bronchus", "all races (includes hispanic)", "males", "ages 50+", "late stage (regional & distant)", "latest 5 year average")

area = "wa"
areatype = "county"
race = "all races (includes hispanic)"
sex = "males"
cancer = "lung & bronchus"
age = "ages 50+"
stage = "late stage (regional & distant)"
year = "latest 5 year average"


incidence_cancer <- function(area, areatype, cancer, race, sex, age, stage, year) {

  req <- create_request("incidencerates")

  resp <- req %>%
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      cancer=handle_cancer(cancer),
      race=handle_race(race),
      sex=handle_sex(sex),
      age=handle_age(age),
      stage=handle_stage(stage),
      year=handle_year(year),
      type="incd",
      sortVariableName="rate",
      sortOrder="default",
      output=1
    )

  # if (!is.null(year) && area == "state") {
  #   req_draft <- req_draft %>%
  #     req_url_query(year=handle_year(year))
  # } else if (!is.null(year) && area != "state") {
  #   cli_abort("Year must be NULL unless you are declaring latest single year (us by state) for AREA is state")
  # }

  if ((cancer == "breast (female)" || cancer == "breast (female in situ)") && (sex == "males" || sex = "both sexes")) {
    cli_abort("For breast cancers, Sex must be Females")
  }

  resp <- resp %>%
    req_perform()

  resp <- process_incidence(resp)


  areatype_map <- c("county" = "County", "hsa" = "Health Service Area", "state" = "State")
  areatype_title <- areatype_map[areatype]

  if (stage == "all stages") {
    resp %>%
      setNames(c(areatype_title, "FIPS", "Age Adjusted Incidence Rate", "Lower 95% CI", "Upper 95% CI", "CI Rank", "Lower CI Rank", "Upper CI Rank", "Annual Average Count", "Recent Trend", "Recent 5 Year Trend", "Trend Lower 95% CI", "Trend Upper 95% CI"))
  } else if (stage == "late stage (regional & distant)") {
    resp %>%
      setNames(c(areatype_title, "FIPS", "Age Adjusted Incidence Rate", "Lower 95% CI", "Upper 95% CI", "CI Rank", "Lower CI Rank", "Upper CI Rank", "Annual Average Count", "Percentage of Cases with Late Stage"))
  }
}