#' Custom print function
#'
#' This custom print function edits the comment on the
#' metadata tibble output for a response object
#'
#' @param x
#'
#' @export
print.cancerprof_tbl <- function(x, ...) {
  # we actually need to figure out how to use pillar here
  cat("\033[38;5;246m# Access metadata with `get_metadata()`\033[39m", "\n")

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
process_metadata <- function(resp) {
  
  resp_data <- resp$data
  resp_metadata <- resp$metadata
  
  class(resp_data) <- c("cancerprof_tbl", class(resp_data))
  attr(resp_data, "metadata") <- resp_metadata
  
  #print(resp_metadata)
  return(resp_data)
}
