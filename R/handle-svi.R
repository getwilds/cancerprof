#' Handles Social Vulnerability Index (SVI) Values to Code
#'
#' This function returns a matching code value for a SVI
#' for the api to use to get data from State Cancer Profiles
#'
#' @param svi One of the following values:
#' - `"Overall"`
#' - `"socioeconomic status"`
#' - `"household characteristics"`
#' - `"racial & ethinic minority status"`
#' - `"housing type & transportation"`.
#'
#' @importFrom rlang is_na
#'
#' @returns A string for its respective SVI variable
#'
#' @noRd
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

  if (is_na(svi_code)) {
    stop("Invalid svi input, please check the documentation for valid inputs")
  }

  return(as.character(svi_code))
}
