#' Process Response Data
#' 
#' This function processes the response data from State Cancer Profiles
#'
#' @param resp A response object
#' 
#' @importFrom httr2 resp_body_string
#' @importFrom dplyr mutate_all na_if
#' @importFrom stringr str_detect
#' 
#' @returns A processed response data frame
#' 
#' @examples 
#' process_response(resp)
process_response <- function(resp) {
  resp_lines <- resp %>% 
    resp_body_string() %>% 
    strsplit("\\n") %>%  unlist() 
  
  index_first_line_break <- which(resp_lines == "")[1]
  index_second_line_break <- which(resp_lines == "")[2]
  
  resp <- resp_lines[(index_first_line_break + 1):(index_second_line_break -1)] %>% 
    paste(collapse = "\n") %>% 
    (\(x) read.csv(textConnection(x), header=TRUE, colClasses = "character"))() %>% 
  
    filter(str_detect(County, "County"))
  
  
    # if (areatype == "county") {
    #   resp <- resp %>%
    #     filter(str_detect(County, "County"))
    # } else if (areatype == "hsa") {
    #   resp <- resp[-1, ]
    #   rownames(resp) <- NULL
    # }
    
    resp <- resp %>% 
      mutate_all(\(x) na_if(x, "N/A")) %>% 
      mutate_all(\(x) na_if(x, "data not available"))
}
