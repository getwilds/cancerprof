#' Access to Trend Data
#'
#' This function returns a data frame about trend data on incidence and
#' mortality from State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA or "seer 9 registeries"
#' @param cancer One of the following values:
#' - `"all cancer sites"`
#' - `"bladder"`
#' - `"brain & ons"`
#' - `"breast (female)"`
#' - `"breast (female in situ)"`
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
#' - `"uterus (corpus & uterus, nos)"`.
#' @param race
#' One of the following values:
#' - `"all Races (includes Hispanic)"`
#' - `"white non-hispanic"`
#' - `"black (non-hispanic)"`
#' - `"american indian / alaska native (non-hispanic)"`
#' - `"asian / pacific islander (non-hispanic)"`
#' - `"hispanic (Any Race)"`.
#' @template param-sex
#' @param age One of the following values:
#' - `"all ages"`
#' - `"ages <50"`
#' - `"ages 50+"`
#' - `"ages <65"`
#' - `"ages 65+"`
#' @param datatype One of the following values:
#' - `"incidence"`
#' - `"mortality"`
#'
#' @importFrom httr2 req_url_query req_perform
#' @importFrom stats setNames
#' @importFrom dplyr mutate across
#' @importFrom tibble as_tibble
#'
#' @returns A data frame with the following columns: Year, Observed, Estimated
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pull_trend_data(
#'   area = "wa",
#'   cancer = "lung & bronchus",
#'   data = "incidence",
#'   race = "All Races (includes Hispanic)",
#'   sex = "both sexes",
#'   age = "all ages"
#' )
#' }
pull_trend_data <- function(area, cancer, race, sex, age, datatype) {
  
  req <- create_request("trend")
  #area = "wa"
  #age = "all ages"
  #cancer = "lung & bronchus"
  #race = "all races (includes hispanic)"
  #sex = "both sexes"
  #datatype = "incidence"
  
  
  trend_url <- "?0"
  area_code <- trend_fips_scp(area)
  age_code <- handle_age(age)
  cancer_code <- handle_cancer(cancer)
  race_code <- handle_race(race)
  sex_code <- handle_sex(sex)
  datatype_code <- handle_trend_datatype(datatype)
  
  trend_url <- paste0(trend_url, "&", area_code, "&999&7599&", age_code, "&", cancer_code, "&", race_code, "&", sex_code, "&0&0&", datatype_code, "&0&1&1&6")
  
  #append the full string of the rest of the URL bc req_url_path_append() will add a "/"
  req <- req %>% 
    req_url_path_append(trend_url)
  
  
  resp <- req %>%
    req_perform()
  
  resp_url <- resp$url
  
  resp_lines <- resp %>%
    resp_body_string() %>%
    strsplit("\\n") %>%
    unlist()
  
  line_length <- length(resp_lines)
  
  index_first_line_break <- which(resp_lines == "")[3]
  index_second_line_break <- which(resp_lines == "")[4]
  
  resp <- resp_lines[
    (index_first_line_break + 2):(index_second_line_break - 1)
  ] %>%
    paste(collapse = "\n") %>%
    (\(x) {
      read.csv(textConnection(x), header = TRUE, colClasses = "character")
    })()
  
  resp_metadata <- c(
    resp_lines[1: (index_first_line_break - 1)], resp_lines[(index_second_line_break + 1): line_length]
  )
  
  resp <- list(metadata = resp_metadata, data = resp)
  
  process_metadata(resp, "trend", resp_url)
  
}
