process_metadata <- function(resp) {
  resp_data <- resp$data
  resp_metadata <- resp$metadata
  
  resp_df <- resp_data %>% as_tibble()
  
  class(resp_df) <- c("my_tbl", class(resp_df))
  
  attr(resp_df, "metadata") <- resp_metadata
  
  print.my_tbl <- function(x) {
    cat("Metadata:", "\n")
    for (i in seq_along(attr(x, "metadata"))) {
      cat(names(attr(x, "metadata"))[i], attr(x, "metadata")[[i]], "\n")
    }
    NextMethod(x)
    invisible(x)
  }
  print.my_tbl(resp_df)
}
