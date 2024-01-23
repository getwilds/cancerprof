#' Access to Social Vulnerability Index (SVI) Data
#' 
#' This function returns a data frame from Social Vulnerability Index (SVI) in State Cancer Profiles
#'
#' @param area A state/territory abbreviation or USA.
#' @param social Either "Overall, "socioeconomic status", "household characteristics", "racial & ethinic minority status", "housing type & transportation"
#' 
#' @returns A data frame with the following columns "County", "FIPS", "Score"
#' 
#' \dontrun{
#' @examples 
#' demo_svi("WA", "overall")
#' demo_svi("usa", "overall")
#' demo_svi("dc", "socioeconomic status")
#' }
demo_svi <- function(area, svi) {
  
  req <- create_request("demographics")
  
  resp <- req %>% 
    req_url_query(
      stateFIPS=fips_scp(area),
      areatype="county",
      topic="svi",
      demo=handle_svi(svi),
      type="manyareacensus",
      sortVariableName="value",
      sortOrder="default",
      output=1
    ) %>% 
    req_perform()
  
  
  resp <- process_response(resp)
  
  resp %>% 
    setNames(c("County", "FIPS", "Score"))
}


demo_svi("WA", "overall")
demo_svi("usa", "overall")
demo_svi("dc", "socioeconomic status")

