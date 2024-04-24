#' Create Request to State Cancer Profile Data
#'
#' This creates a request to the State Cancer Profiles URL
#'
#' @param topic Either "demographics", "risk", "incidencerates", "deathrates", "trend"
#'
#' @importFrom httr2 request
#'
#' @returns returns the HTTP method with the state cancer profiles
#' url and the http path
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' create_request("demographics")
#' create_request("risk")
#' }
create_request <- function(topic) {
  url <- "https://statecancerprofiles.cancer.gov/"
  trend_url <- "https://statecancerprofiles.cancer.gov/historicaltrend/data.php/historicaltrend.csv?"
  url_end <- "/index.php"
  
  #if pulling trend data, append "/data.php/historicaltrend.csv"
  if (topic == "trend") {
    request(trend_url)
  } else {
    url <- paste0(url, topic, url_end)
    
    request(url)
  }
}
