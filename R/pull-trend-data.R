
pull_trend_data <- function(area, areatype, cancer, race, sex, age, stage, year) {

  
  # url <- "https://statecancerprofiles.cancer.gov/historicaltrend/data.php/historicaltrend.csv"
  url <- "https://statecancerprofiles.cancer.gov/historicaltrend/data.php/historicaltrend.csv?0&9900&999&7599&001&001&00&0&0&0&1&0&1&1&6"

  req <- request(url)
  
  req <- create_request("trend")
  
  #?0&9900&999&7599&001&001&00&0&0&0&1&0&1&1&6
  #req_url_path_append()
  
  # seems to be always "0" if no new lines are added
  # number of additional trend lines
  # "99" + FIPS or "5099 for SEER 9
  # 999
  # 7599.  UTAH = 2649 NEW YORK = 4636, NEW MEXICO = 2335, NEW JERSEY = 4434, MASSACHUSETTS = 4725, LOUISIANA = 4322, KENTUCKY = 4221, IOWA = 2219, IDAHO = 4516, HAWAII = 2115, CONNECTICUT = 0209, CALIFORNIA = 9706 
  # age
  # cancer
  # race
  # sex
  # 
  # 
  # incidence == 1, mortality == 2, data suppressed == ? 
  #
  #
  # total number of trend lines
  # output == 6

  resp <- req %>%
    req_perform()
  
  resp_lines <- resp %>%
    resp_body_string() %>%
    strsplit("\\n") %>%
    unlist()
  
  resp_lines
  
  
  index_first_line_break <- which(resp_lines == "")[3]
  index_second_line_break <- which(resp_lines == "")[4]
  
  resp <- resp_lines[
    (index_first_line_break + 2):(index_second_line_break - 1)
  ] %>%
    paste(collapse = "\n") %>%
    (\(x) {
      read.csv(textConnection(x), header = TRUE, colClasses = "character")
    })()
  
  process_metadata(resp, "incidence", resp_url)
}
