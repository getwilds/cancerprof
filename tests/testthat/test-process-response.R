#' Test Process Response
#'
#' This testthat file test the process-response function

# sets up the testing environment with an example resp
resp <- dput_resp_demo()
result <- process_response(resp)

# process response should return a data frame
test_that("process_response should return a data frame", {
  skip_on_cran()
  expect_true(is.data.frame(result))
})

# process response should have the correct parameter
test_that("process response should have resp as an argument", {
  skip_on_cran()
  expect_error(
    process_response()
  )
})

# process response data should start on the column names and end with data
# containing a FIPS value
test_that("process response outputs data from the correct line", {
  skip_on_cran()
  area_headers <- c("County", "State", "Health.Service.Area")

  expect_true(any(colnames(result) %in% area_headers))

  expect_true("FIPS" %in% colnames(result))
  expect_true(!is.na(result[nrow(result), "FIPS"]))
})

# process response filters out correct data
test_that("process response filters out United States and state names", {
  skip_on_cran()
  # Filters out "United States" from all results
  expect_false(any(result[1] == "United States"))

  # Filters out State names from County and HSA
  county_hsa <- c("County", "Health.Service.Area")
  if (colnames(result)[1] %in% county_hsa) {
    expect_false(any(result[1] == state.name))
  }
})
