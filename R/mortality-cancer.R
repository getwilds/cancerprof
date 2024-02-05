#' Access to Cancer Mortality Data
#' 
#' This function returns a data frame from Cancer Mortality in State Cancer Profiles
#' 
#' This is the details section this is where the finer details of the function are explained
#'
#' @param area A state/territory abbreviation or USA.
#' @param areatype Either "county", "hsa" (Health service area), or "state"
#' @param cancer Either "all cancer sites","bladder", "brain & ons", "breast (female)", "cervix", 
#'                      "childhood (ages <15, all sites)", "childhood (ages <20, all sites)", "colon & rectum", "esophagus", 
#'                      "kidney & renal pelvis", "leukemia", "liver & bile duct", "lung & bronchus", "melanoma of the skin", 
#'                      "non-hodgkin lymphoma", "oral cavity & pharynx", "ovary", "pancreas", "prostate", "stomach", 
#'                      "thyroid", "uterus (corpus & uterus, nos)"
#' @param race One of the following values: "all races (includes hispanic)", "white (non-hispanic)", 
#'                                          "black (non-hispanic)", "amer. indian / ak native (non-hispanic)", 
#'                                          "asian / pacific islander (non-hispanic)","hispanic (any race)"
#' @param sex Either "both sexes", "males", "females"
#' @param age Either "all ages", "ages <50", "ages 50+", "ages <65", "ages 65+", ages <15, ages <20
#' @param year Either "latest 5 year average", "latest single year (us by state)"
#' 
#' @returns A data frame with the following columns "State", "FIPS", "Percent", "Lower 95% CI", "Upper 95% CI", "Number of Respondents"
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' mortality_cancer("wa", "county", "all cancer sites", "black (non-hispanic)", 
#'                  "both sexes", "ages 65+")
#' mortality_cancer("ca", "hsa", "lung & bronchus", "all races (includes hispanic)", 
#'                  "males", "ages 50+")
#' mortality_cancer("usa", "state", "prostate", "all races (includes hispanic)", 
#'                  "males", "ages 50+")
#' mortality_cancer(area="wa", areatype="county", cancer="ovary", race="all races (includes hispanic)", 
#'                  sex="females", age="ages 50+")
#' mortality_cancer("ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)", 
#'                  "males", "ages <20")
#' }
mortality_cancer <- function(area, areatype, cancer, race, sex=NULL, age, year="latest 5 year average") {
  female_cancer <- c("breast (female)", "ovary", "uterus (corpus & uterus, nos)")
  
  childhood_cancer <- c("childhood (ages <15, all sites)", "childhood (ages <20, all sites)")
  
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
  
  
  req <- create_request("deathrates")
  
  resp <- req %>%
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype=tolower(areatype),
      cancer=handle_cancer(cancer),
      race=handle_race(race),
      age=handle_age(age),
      year=handle_year(year),
      type="death",
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
  
  resp <- process_mortality(resp)
  
  areatype_map <- c("county" = "County", "hsa" = "Health_Service_Area", "state" = "State")
  areatype_title <- areatype_map[areatype]
  
  areacode_map <- c("county" = "FIPS", "state" = "FIPS", "hsa" = "HSA_Code")
  areacode_title <- areacode_map[areatype]
  
  resp %>% 
    setNames(c(areatype_title, areacode_title, "Met Healthy People Objective of ***?",  "Age Adjusted Death Rate", "Lower 95% CI Rate", "Upper 95% CI Rate", 
               "CI Rank", "Lower CI Rank", "Upper CI Rank", "Annual Average Count", "Recent Trend", 
               "Recent 5 Year Trend", "Lower 95% CI Trend", "Upper 95% CI Trend"))
}

