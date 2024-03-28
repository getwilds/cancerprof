#' Process Metadata
#'
#' This function ...
#'
#' @param x
#'
#' @export
print.my_tbl <- function(x, ...) {
  cat("Metadata:", "\n")
  for (i in seq_along(attr(x, "metadata"))) {
    cat(names(attr(x, "metadata"))[i], attr(x, "metadata")[[i]], "\n")
  }
  NextMethod(x, ...)
}

#' Process Metadata
#'
#' This function ...
#'
#' @param resp A response object
process_metadata <- function(resp) {
  
  resp_data <- resp$data
  resp_metadata <- resp$metadata
  
  resp_data <- resp_data %>% as_tibble()
  class(resp_data) <- c("my_tbl", class(resp_data))
  attr(resp_data, "metadata") <- resp_metadata
  print(resp_data)
  return(resp_data)
}
