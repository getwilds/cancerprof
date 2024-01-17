#' Handles Race Values to Code
#' 
#' This function returns a matching code value for a race for the api to use to get data from State Cancer Profiles
#'
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' 
#' @returns A data frame with the following columns "County", "Value (Percent)", "Households (with >1 Person Per Room)", "Rank within US (of 3143 counties)"
#' 
#' @examples 
#' handle_race("all races (includes hispanic)"), handle_race(race)

handle_race <- function(race) {
  
  race <- tolower(race)
  
  race_mapping <- c(
    "all races (includes hispanic)" = "00",
    "white (includes hispanic)" = "01",
    "white non-hispanic" = "07",
    "black" = "02",
    "black (includes hispanic)" = "02",
    "amer. indian/alaskan native (includes hispanic)" = "03",
    "all ages, asian or pacific islander (includes hispanic)" = "04",
    "hispanic (any race)" = "05"
  )
  
  code <- race_mapping[race]
  
  if (is.null(code)) {
    stop("Invalid input")
  }
  
  return(as.character(code))
}
