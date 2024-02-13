#' Test demo-population
#' 
#' This testthat file test the demo-population function
#' 
#tests class and typeof output
test_that("Output data type is correct", {
  output <- demo_population("WA", "county", "asian/pacific islander", sex="females")
  
  expect_true(inherits(output, "data.frame"))
})

#Ensures that variables are present and working on SCP
population_options <- list(
  age1 = demo_population("wa", "county", "age under 18", "all races (includes hispanic)", "both sexes"),
  age2 = demo_population("wa", "county", "ages 60 and over", "all races (includes hispanic)", "both sexes"),
  age3 = demo_population("wa", "county", "age 18-39", sex = "both sexes"),
  age4 = demo_population("wa", "county", "age 40-64", sex = "both sexes"),
  age5 = demo_population("wa", "county", "ages 40 and over"),
  age6 = demo_population("wa", "county", "ages 50 and over"),
  race1 = demo_population("wa", "county", "foreign born", "all races (includes hispanic)", "both sexes"),
  sex1 = demo_population("wa", "county", "males", race = "all races (includes hispanic)"),
  sex2 = demo_population("wa", "county", "females", race = "all races (includes hispanic)")
)

for (option_name in names(population_options)) {
  test_that("demo-population returns non-empty data frame", {
    option <- population_options[[option_name]]
    expect_true(is.data.frame(option))
  })
}

race_options <- c("american indian/alaska native", "asian/pacific islander", 
           "black", "hispanic", "non-hispanic (origin recode)")

for (option in race_options) {
  test_that("demo-population returns non-empty data frame", {
    expect_true(is.data.frame(demo_population("wa", "county", option, sex="both sexes")))
  })
}

#demo-population must have 5 columns
test_that("demo-population has correct number of columns", {
  df <- demo_population("WA", "county", "asian/pacific islander", sex="females")
  expected_columns <- 5
  expect_equal(ncol(df), expected_columns)
})

#test error handling
test_that("demo-population handles invalid population parameters", {
  expect_error(
    demo_population("wa", "county", "ages 40 and over", race="all races includes hispanic"),
    "ages 40 and over and ages 50 and over, Race and Sex must be NULL"
  )
  expect_error(
    demo_population("wa", "county", "age under 18"),
    "for age under 18 and ages 60 and over, Sex and Race must not be NULL"
  )
  expect_error(
    demo_population("wa", "county", "foreign born"),
    "for foreign born, race and sex must not be NULL"
  )
})

#parameter
test_that("demo-population has correct parameters", {
  expect_error(demo_population())
})