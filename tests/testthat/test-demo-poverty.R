#' Test demo-poverty
#'
#' This testthat file test the demo-poverty function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- demo_poverty("wa", "county", "persistent poverty")

  expect_true(inherits(output, "data.frame"))
})

# Ensures that variables are present and working on SCP
poverty_options <- list(
  pov1 = demo_poverty(
    "wa",
    "county",
    "persistent poverty"
  ),
  pov2 = demo_poverty(
    "wa",
    "county",
    "families below poverty",
    "black"
  ),
  pov3 = demo_poverty(
    "wa",
    "county",
    "persons below poverty",
    "black",
    "both sexes"
  ),
  pov4 = demo_poverty(
    "wa",
    "county",
    "persons < 150% of poverty"
  )
)

for (option_name in names(poverty_options)) {
  test_that("demo-poverty returns non-empty data frame", {
    skip_on_cran()
    option <- poverty_options[[option_name]]
    expect_true(is.data.frame(option))
  })
}

# demo-poverty must have 5 columns
test_that("demo-poverty has correct number of columns", {
  skip_on_cran()
  df1 <- demo_poverty("wa", "county", "persistent poverty")
  df2 <- demo_poverty("wa", "county", "families below poverty", "black")
  expected_columns1 <- 3
  expected_columns2 <- 5
  expect_equal(ncol(df1), expected_columns1)
  expect_equal(ncol(df2), expected_columns2)
})

# test error handling
test_that("demo-poverty handles invalid poverty parameters", {
  skip_on_cran()
  expect_error(
    demo_poverty("wa", "hsa", "persistent poverty"),
    "For persistent poverty, areatype must be county"
  )
  expect_error(
    demo_poverty("wa", "county", "families below poverty"),
    "for families below poverty, Sex must be NULL and Race must not be NULL"
  )
  expect_error(
    demo_poverty("wa", "county", "persons below poverty", "black"),
    "for persons below poverty, Sex and Race must not be NULL"
  )
})

# parameter
test_that("demo-poverty has correct parameters", {
  expect_error(demo_poverty())
})
