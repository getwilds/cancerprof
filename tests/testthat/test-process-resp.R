#' Test Process Response
#'
#' This testthat file test the process-response function

# sets up the testing environment with an example resp
resp_list <- list(
  demographics = dput_resp_demo(),
  risks = dput_resp_risk(),
  incidence = dput_resp_incd(),
  mortality = dput_resp_mortality()
)

for (resp_name in names(resp_list)) {
  resp <- resp_list[[resp_name]]
  result <- process_resp(resp, resp_name)

  # process response should return a data frame
  test_that("process_response should return a data frame", {
    skip_on_cran()
    expect_true(is.data.frame(result$data))
  })

  # process response should have the correct parameter
  test_that("process response should have resp as an argument", {
    skip_on_cran()
    expect_error(
      process_resp()
    )
  })

  # process response data should start on the column names and end with data
  # containing a FIPS value
  test_that("process response outputs data from the correct line", {
    skip_on_cran()
    area_headers <- c("County", "State", "Health.Service.Area")

    expect_true(any(colnames(result$data) %in% area_headers))

    expect_true("FIPS" %in% colnames(result$data))
    expect_true(!is.na(result$data[nrow(result$data), "FIPS"]))
  })

  # process response filters out correct data
  test_that("process response filters out United States and state names", {
    skip_on_cran()
    # Filters out "United States" from all results
    expect_false(any(result$data[1] == "United States"))

    # Filters out State names from County and HSA
    county_hsa <- c("County", "Health.Service.Area")
    if (colnames(result$data)[1] %in% county_hsa) {
      expect_false(any(result$data[1] == state.name))
    }
  })
}
