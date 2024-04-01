#' Custom print function
#'
#' This custom print function processes the
#' metadata output for a response object
#'
#' @param x
#'
#' @export
print.cancerprof_tbl <- function(x, ...) {
  # cat("Metadata:", "\n")
  # for (i in seq_along(attr(x, "metadata"))) {
  #   cat(names(attr(x, "metadata"))[i], attr(x, "metadata")[[i]], "\n")
  # }
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
  
  print(resp_data)
  return(resp_data)
}
