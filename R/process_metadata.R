#' Custom print function
#'
#' This custom print function edits the comment on the
#' metadata tibble output for a response object
#'
#' @param x
#'
#' @export
print.cancerprof_tbl <- function(x, ...) {
  original_url <- attributes(x)$url
  modified_url <- gsub("&output=1", "#results", original_url)
  
  cli_div(theme = list(
    span.cancerprof_class = list(color = "darkgray")))
  
  cli_par()
  cli_text(
    "{.href [# Click to view this query on State Cancer Profiles](", modified_url, ")}"
  )
  cli_text("{.cancerprof_class # Access metadata with `get_metadata()`}")
  cli_end()
  
  NextMethod(x, ...)
}

#' Process Metadata
#'
#' This function sets the class of the response data 
#' to use the custom print function
#'
#' @param resp A response object
#' 
#' @returns A response object with Metadata and a tibble
#' 
#' @examples
#' \dontrun{
#' process_metadata(resp)
#' }
process_metadata <- function(resp, data_topic, resp_url) {
  
  resp_data <- resp$data
  resp_metadata <- resp$metadata
  
  #remove new lines
  resp_metadata <- resp_metadata[!grepl("^\\s*$", resp_metadata)]
  
  class(resp_data) <- c("cancerprof_tbl", class(resp_data))
  attr(resp_data, "metadata") <- resp_metadata
  
  attr(resp_data, "data_topic") <- data_topic
  
  attr(resp_data, "url") <- resp_url
  return(resp_data)
}
