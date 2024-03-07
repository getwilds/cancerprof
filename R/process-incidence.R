#' Process Cancer Incidence Response Data
#'
#' This function processes the Cancer Incidence response data from State Cancer Profiles
#'
#' @param resp A response object
#'
#' @importFrom httr2 resp_body_string
#' @importFrom dplyr mutate_all na_if filter mutate rename
#' @importFrom rlang sym :=
#' @importFrom utils read.csv data
#' @importFrom stringr str_replace_all
#'
#' @returns A processed response data frame
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' process_incidence(resp)
#' }
process_incidence <- function(resp) {
  nenv <- new.env()
  data("state", envir = nenv)
  state.name <- nenv$state.name

  resp_lines <- resp %>%
    resp_body_string() %>%
    strsplit("\\n") %>%
    unlist()

  index_first_line_break <- which(resp_lines == "")[4]
  index_second_line_break <- which(resp_lines == "")[5]

  resp <- resp_lines[(index_first_line_break + 1):(index_second_line_break - 1)] %>%
    paste(collapse = "\n") %>%
    (\(x) read.csv(textConnection(x), header = TRUE, colClasses = "character"))()


  column <- c("County", "Health.Service.Area", "State")[c("County", "Health.Service.Area", "State") %in% colnames(resp)]

  resp <- resp %>%
    filter(!!sym(column) != "US (SEER+NPCR)(1)") %>%
    mutate(!!sym(column) := str_replace_all(!!sym(column), "\\(\\d+\\)", ""))


  if (column %in% c("Health.Service.Area", "County")) {
    resp <- resp %>%
      filter(!(!!sym(column) %in% state.name))
  }
  resp
}
