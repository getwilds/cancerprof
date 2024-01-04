library(httr2)
library(dplyr)

req <- request("https://statecancerprofiles.cancer.gov/demographics/index.php")

# Most APIs expect small amounts of data in either form or json encoded:
x <- req %>% 
  req_url_query(
    stateFIPS=53,
    areatype="county",
    topic="ed",
    demo="00004",
    type="manyareacensus",
    sortVariableName="value",
    sortOrder="desc",
    output=1
  ) %>% 
  req_perform()

x %>% 
  resp_body_string() %>% 
  cat(file = "~/Downloads/dd5.txt", sep = "\n")

string_x <- x %>% 
  resp_body_string() %>% 
  strsplit("\\n") %>%  unlist() 

print(string_x)

df <- data.frame(lines = string_x)

index_first_line_break <- which(df$lines == "")[1]
index_second_line_break <- which(df$lines == "")[2]

new_data <- df %>%
  slice((index_first_line_break + 1):(index_second_line_break - 1)) %>%
  pull(lines)


# Split each line into a list of values
split_data <- strsplit(new_data, ",")

# Transpose the list of values
transposed_data <- t(do.call(cbind, split_data))

# Convert the transposed data into a dataframe
df <- as.data.frame(transposed_data, stringsAsFactors = FALSE)

# Use the first row as column names
colnames(df) <- df[1, ]

# Remove the first row (column names)
df <- df[-1, ]

# Print the dataframe
print(df)


file_path <- "~/Downloads/new_data.csv"
  
write.csv(df, file = file_path, row.names = FALSE)





View(df)
