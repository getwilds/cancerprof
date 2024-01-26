#' Handles Social Vulnerability Index (SVI) Values to Code
#' 
#' This function returns a matching code value for a SVI for the api to use to get data from State Cancer Profiles
#'
#' @param svi One of the following values: 
#'                              "overall", "socioeconomic status", "household characteristics",
#'                              "racial & ethinic minority status", "housing type & transportation"
#' 
#' @returns A string for its respective SVI variable
#' 
#' @examples 
#' \dontrun{
#' handle_svi("overall")
#' }
handle_svi <- function(svi) {
  svi <- tolower(svi)
  
  svi_mapping <- c(
    "overall" = "03010",
    "socioeconomic status" = "03011",
    "household characteristics" = "03012",
    "racial & ethinic minority status" = "03013",
    "housing type & transportation" = "03014"
  )
  
  svi_code <- svi_mapping[svi]
  
  if (is.null(svi_code)) {
    stop("Invalid input")
  }
  
  return(as.character(svi_code))
}
