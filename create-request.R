library(httr2)

#' Create Request to State Cancer Profile Data
#' 
#' This creates a request to the State Cancer Profiles URL
#'
#' @importFrom httr2 request
#' 
#' @returns returns the HTTP method with the state cancer profiles url and the http path
#' 
#' @examples 
#' create_request()

create_request <- function(url = "https://statecancerprofiles.cancer.gov/demographics/index.php") {
  return(request(url))
}
