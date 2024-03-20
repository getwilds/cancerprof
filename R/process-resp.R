#' Process Response Data
#'
#' This function processes the response data from State Cancer Profiles
#'
#' @param resp A response object
#' @param topic One of the following values:
#' - "demographics"
#' - "risks"
#' - "incidence"
#' - "mortality"
#'
#' @importFrom httr2 resp_body_string
#' @importFrom dplyr mutate_all na_if filter
#' @importFrom rlang sym
#' @importFrom utils read.csv data
#' @importFrom stringr str_trim
#'
#' @returns A processed response data frame
#'
#' @noRd
#'
#' @examples
#' process_resp(resp, "demographics")
#' 
process_resp <- function(resp, topic) {
  if (httr2::resp_content_type(resp) != "text/csv") {
    cli_abort("Invalid input, please check documentation for valid arguments.")
  }

  nenv <- new.env()
  data("state", envir = nenv)
  state_name <- nenv$state.name

  resp_lines <- resp %>%
    resp_body_string() %>%
    strsplit("\\n") %>%
    unlist()

  if (topic == "demographics") {
    index_first_line_break <- which(resp_lines == "")[1]
    index_second_line_break <- which(resp_lines == "")[2]
  } else if (topic == "risks") {
    index_first_line_break <- which(resp_lines == "")[3]
    index_second_line_break <- which(resp_lines == "")[4]
  } else if (topic == "incidence" || topic == "mortality") {
    index_first_line_break <- which(resp_lines == "")[4]
    index_second_line_break <- which(resp_lines == "")[5]
  } else {
    cli_abort("Incorrect topic argument, please ensure that correct.")
  }

  resp <- resp_lines[
    (index_first_line_break + 1):(index_second_line_break - 1)
  ] %>%
    paste(collapse = "\n") %>%
    (\(x) {
      read.csv(textConnection(x), header = TRUE, colClasses = "character")
    })()

  column <- c(
    "Health.Service.Area",
    "County",
    "State"
  )[c(
    "Health.Service.Area",
    "County",
    "State"
  ) %in% colnames(resp)]

  resp <- resp %>%
    filter(!!sym(column) != "United States")

  if (column %in% c("Health.Service.Area", "County")) {
    resp <- resp %>%
      filter(!(!!sym(column) %in% state_name))
  }
  resp %>%
    mutate_all(stringr::str_trim) %>%
    mutate_all(\(x) na_if(x, "N/A")) %>%
    mutate_all(\(x) na_if(x, "data not available")) %>%
    mutate_all(\(x) na_if(x, "*"))
}
