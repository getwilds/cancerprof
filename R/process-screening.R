library(cdlTools)
library(cli)
library(dplyr)
library(httr2)
library(magrittr)
library(rlang)
library(stringr)
#' Process Response Data
#' 
#' This function processes the response data from State Cancer Profiles
#'
#' @param resp A response object
#' @param area Either "wa" or "NULL"
#' 
#' @importFrom httr2 resp_body_string
#' @importFrom dplyr mutate_all na_if
#' @importFrom rlang sym
#' 
#' @returns A processed response data frame
#' 
#' \dontrun{
#' @examples 
#' process_screening(resp)
#' }
process_screening <- function(resp) {
  resp_lines <- resp %>%
    resp_body_string() %>% 
    strsplit("\\n") %>%  unlist() 
  
  index_first_line_break <- which(resp_lines == "")[3]
  index_second_line_break <- which(resp_lines == "")[4]
  
  resp <- resp_lines[(index_first_line_break + 1):(index_second_line_break -1)] %>% 
    paste(collapse = "\n") %>% 
    (\(x) read.csv(textConnection(x), header=TRUE, colClasses = "character"))()
  
  column <- c("County", "State")[c("County", "State") %in% colnames(resp)]
  
  resp <- resp %>% 
    filter(!!sym(column) != "United States")
  
  # if(column %in% c("Health.Service.Area", "County")) {
  #   resp <- resp %>%
  #     filter(!(!!sym(column) %in% state.name))
  # }
  resp %>%   
    mutate_all(\(x) na_if(x, "N/A")) %>% 
    mutate_all(\(x) na_if(x, "data not available"))
}
