#' Create Request to State Cancer Profile Data
#' 
#' This creates a request to the State Cancer Profiles URL
#' 
#' @param topic Either "demographics", "risk", "incidencerates", "deathrates"
#' 
#' @importFrom httr2 request
#' 
#' @returns returns the HTTP method with the state cancer profiles url and the http path
#' 
#' @examples
#' \dontrun{
#' create_request("demographics")
#' create_request("risk")
#' }
create_request <- function(topic) {
  url = "https://statecancerprofiles.cancer.gov/"
  url_end = "/index.php"
  
  url = paste0(url, topic, url_end)
  
  req <- request(url)
}
