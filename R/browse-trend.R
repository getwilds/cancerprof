# https://statecancerprofiles.cancer.gov/historicaltrend/index.php?0&9953&999&7599&136&071&48&2&0&0&1&1&1&1#results
# 
# req_url_query(https://statecancerprofiles.cancer.gov/historicaltrend/index.php?0&9953&999&7599&136&071&48&2&0&0&1&1&1&1&6) %>% 
#   req_perform()
# 
# browseURL(paste0(https://statecancerprofiles.cancer.gov/historicaltrend/index.php?0&9953&999&7599&136&071&48&2&0&0&1&1&1&1#results))
#                  
#                  
#                  
# req <- "https://statecancerprofiles.cancer.gov/historicaltrend/index.php"
#  
# api_arguments <- "0&9953&999&7599&136&071&48&2&0&0&1&1&1&1&6"
#  
#  resp <- req %>%
#    req_url_query(
#      stateFIPS = fips_scp(area),
#      areatype = tolower(areatype),
#      topic = "crowd",
#      demo = handle_crowding(crowding),
#      race = handle_race(race),
#      type = "manyareacensus",
#      sortVariableName = "value",
#      sortOrder = "default",
#      output = 1
#    ) %>%
#    req_perform()
#  
#  
#  test_ur_fail <- "https://statecancerprofiles.cancer.gov/historicaltrend/index.php?0&9953&999&7599&136&071&48&2&0&0&1&1&1&1&6"
#  req <- "https://statecancerprofiles.cancer.gov/historicaltrend/data.php/historicaltrend.csv?0&9953&999&7599&136&071&07&2&0&0&1&1&1&1&6"
#  req <- request("https://statecancerprofiles.cancer.gov/historicaltrend/index.php?0&9953&999&7599&136&071&48&2&0&0&1&1&1&1&6")
#  
# resp <- req %>%
#   req_perform() %>% 
#   resp_body_string()
# 
# 
# if (httr2::resp_content_type(resp) != "text/csv") {
#   cli_abort("Invalid input, please check documentation for valid arguments.")
# }
 
 