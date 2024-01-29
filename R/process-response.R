#' Process Response Data
#' 
#' This function processes the response data from State Cancer Profiles
#'
#' @param resp A response object
#' 
#' @importFrom httr2 resp_body_string
#' @importFrom dplyr mutate_all na_if filter
#' @importFrom rlang sym
#' @importFrom utils read.csv data
#' 
#' @returns A processed response data frame
#' 
#' @examples 
#' \dontrun{
#' process_response(resp)
#' }
process_response <- function(resp) {
  
  nenv <- new.env()
  data("state", envir = nenv)
  state.name <- nenv$state.name
  
  resp_lines <- resp %>%
    resp_body_string() %>% 
    strsplit("\\n") %>%  unlist() 
  
  index_first_line_break <- which(resp_lines == "")[1]
  index_second_line_break <- which(resp_lines == "")[2]
  
  resp <- resp_lines[(index_first_line_break + 1):(index_second_line_break -1)] %>% 
    paste(collapse = "\n") %>% 
    (\(x) read.csv(textConnection(x), header=TRUE, colClasses = "character"))()

    
  column <- c("Health.Service.Area", "County", "State")[c("Health.Service.Area", "County", "State") %in% colnames(resp)]
  
  resp <- resp %>% 
    filter(!!sym(column) != "United States")
    
  if(column %in% c("Health.Service.Area", "County")) {
    resp <- resp %>%
      filter(!(!!sym(column) %in% state.name))
  }
  resp %>%   
    mutate_all(\(x) na_if(x, "N/A")) %>% 
    mutate_all(\(x) na_if(x, "data not available"))
}
