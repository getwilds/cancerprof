#' Process Cancer Mortality Response Data
#' 
#' This function processes the Cancer Mortality response data from State Cancer Profiles
#'
#' @param resp A response object
#' 
#' @importFrom httr2 resp_body_string
#' @importFrom dplyr mutate_all na_if filter mutate rename
#' @importFrom rlang sym
#' @importFrom utils read.csv data
#' 
#' @returns A processed response data frame
#' 
#' @examples
#' \dontrun{
#' process_mortality(resp)
#' }


#Health.Service.Area.HSA_Code State.FIPS
process_mortality <- function(resp) {
  
  nenv <- new.env()
  data("state", envir = nenv)
  state.name <- nenv$state.name
  
  resp_lines <- resp %>%
    resp_body_string() %>% 
    strsplit("\\n") %>%  unlist() 
  
  index_first_line_break <- which(resp_lines == "")[4]
  index_second_line_break <- which(resp_lines == "")[5]
  
  resp <- resp_lines[(index_first_line_break + 1):(index_second_line_break -1)] %>% 
    paste(collapse = "\n") %>% 
    (\(x) read.csv(textConnection(x), header=TRUE, colClasses = "character"))()
  
  
  column <- c("County", "State")[c("County", "State") %in% colnames(resp)]
  
  resp <- resp %>% 
    filter(!!sym(column) != "United States")
  
  resp %>%   
    mutate_all(\(x) na_if(x, "N/A")) %>% 
    mutate_all(\(x) na_if(x, "data not available"))
}
