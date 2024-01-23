#' Handles Race Values to Code
#' 
#' This function returns a matching code value for a race for the api to use to get data from State Cancer Profiles
#'
#' @param race One of the following values: "All Races (includes Hispanic)", "white (includes hispanic)" = "01",
#'              "white non-hispanic","black","black (includes hispanic)", "amer. indian/alaskan native (includes hispanic)",
#'              "asian or pacific islander (includes hispanic)","hispanic (any race)
#' 
#' @returns A string for its respective race
#' 
#' \dontrun{
#' @examples 
#' handle_race("all races (includes hispanic)")
#' handle_race(race)
#' }
handle_race <- function(race) {
  
  race <- tolower(race)
  
  race_mapping <- c(
    #demographic
    "all races (includes hispanic)" = "00",
    "white (includes hispanic)" = "01",
    "white non-hispanic" = "07",
    "black" = "02",
    "black (includes hispanic)" = "02",
    "amer. indian/alaskan native (includes hispanic)" = "03",
    "all ages, asian or pacific islander (includes hispanic)" = "04",
    "hispanic (any race)" = "05",
    
    #screening and risk factors
    "white (non-hispanic)" = "07",
    "black (non-hispanic)" = "28",
    "amer. indian / ak native (non-hispanic)" = "38",
    "asian / pacific islander (non-hispanic)" = "48"
    
  )
  
  code <- race_mapping[race]
  
  if (is.null(code)) {
    stop("Invalid input")
  }
  
  return(as.character(code))
}
