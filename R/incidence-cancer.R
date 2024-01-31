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
#' @param age Either "all ages", "ages <50", "ages 50+", "ages <65", "ages 65+", ages <15, ages <20
#' @param stage Either "all stages" or "late stage (regional & distant)"
#' 
#' @returns A data frame with the following columns "State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' incidence_cancer("wa", "county", "all cancer sites", "black (non-hispanic)", "both sexes", "ages 65+", "all stages")
#' incidence_cancer("wa", "county", "lung & bronchus", "all races (includes hispanic)", "males", "ages 50+", "late stage (regional & distant)")
#' incidence_cancer("usa", "state", "lung & bronchus", "all races (includes hispanic)", "males", "ages 50+", "late stage (regional & distant)")
#' incidence_cancer(area="wa", areatype="county", cancer="ovary", race="all races (includes hispanic)", sex="females", age="ages 50+", stage="late stage (regional & distant)")
#' incidence_cancer("ca", "hsa", "prostate", "all races (includes hispanic)", "males", "ages 50+", "all stages")
#' incidence_cancer("ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)", "males", "ages <20", "all stages")


incidence_cancer(area="wa", areatype="county", cancer="all cancer sites", race="black (non-hispanic)", sex="both sexes", age="ages 50+", stage="all stages")

area = "wa"
areatype = "county"
race = "all races (includes hispanic)"
sex = "males"
cancer = "lung & bronchus"
age = "ages 50+"
stage = "late stage (regional & distant)"
year = "latest 5 year average"


incidence_cancer <- function(area, areatype, cancer, race, sex=NULL, age, stage, year="latest 5 year average") {
  
  allstage_cancer <- c("all cancer sites", "breast (female in situ)", "childhood (ages <15, all sites)", 
                       "childhood (ages <20, all sites)", "leukemia")
  
  female_cancer <- c("breast (female)", "breast (female in situ)", "ovary", "uterus (corpus & uterus, nos)")
  
  childhood_cancer <- c("childhood (ages <15, all sites)", "childhood (ages <20, all sites)")
  
  if ((cancer %in% allstage_cancer) && stage == "late stage (regional & distant)") {
    cli_abort("For this cancer type, stage must be all stages")
  }
  
  if ((cancer %in% female_cancer) && (sex == "males" || sex == "both sexes")) {
    cli_abort("For this cancer type, sex must be females")
  } else if (cancer == "prostate" && (sex == "females" || sex == "both sexes")) {
    cli_abort("For prostate cancer, sex must be males.")
  }
  
  if (cancer == "childhood (ages <15, all sites)" && age != "ages <15") {
    cli_abort("For childhood (ages <15, all sites), age must be ages <15")
  } else if (cancer == "childhood (ages <20, all sites)" && age !="ages <20") {
    cli_abort("For childhood (ages <20, all sites), age must be ages <20")
  } else if ((!cancer %in% childhood_cancer) && (age == "ages <15" || age == "ages <20")) {
    cli_abort("For this cancer type, age cannot be ages <15 or ages <20")
  }
  
  req <- create_request("incidencerates")
  
  resp <- req %>%
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      cancer=handle_cancer(cancer),
      race=handle_race(race),
      age=handle_age(age),
      stage=handle_stage(stage),
      year=handle_year(year),
      type="incd",
      sortVariableName="rate",
      sortOrder="default",
      output=1
    )
  
  if(!is.null(sex)) {
    resp <- resp %>%
      req_url_query(sex=handle_sex(sex))
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
