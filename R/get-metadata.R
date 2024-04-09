
# result <- demo_crowding(
#   area = "WA",
#   areatype = "county",
#   crowding = "household with >1 person per room",
#   race = "All Races (includes Hispanic)"
# )

#helper function to remove key from values
extract_values <- function(key, resp_metadata) {
  values <- resp_metadata[grep(key, resp_metadata)]
  values <- gsub(paste0("^\\s*", key, ":?\\s*"), "", values)
  return(values)
}


get_metadata <- function(input_tbl) {
  resp_metadata <- attr(input_tbl, "metadata")
  
  resp_metadata <- gsub("\\\"", "", resp_metadata)

  report_header <- resp_metadata[1]
  data_report <- c(resp_metadata[2], resp_metadata[3], resp_metadata[4])
  sortedby <- extract_values("Sorted by", resp_metadata)
  createdby <- extract_values("Created by", resp_metadata)
  data_sources <- extract_values("Source", resp_metadata)
  data_dictionary <- extract_values("For more information about", resp_metadata)
  data_limitations <- extract_values("Data for", resp_metadata)


  demo_metadata_list <- list(
    report_header = report_header,
    data_report = data_report,
    sortedby = sortedby,
    createdby = createdby,
    data_sources = data_sources,
    data_dictionary = data_dictionary,
    data_limitations = data_limitations
  )
  return(demo_metadata_list)
}
