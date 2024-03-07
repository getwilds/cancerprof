#' Test risk-smoking
#'
#' This testthat file test the risk-smoking function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- risk_smoking("smoking laws (any)")

  expect_true(inherits(output, "data.frame"))
})

# Ensures that variables are present and working on SCP

# group1
smoking_group1_options <- c(
  "smoking laws (any)",
  "smoking laws (bars)",
  "smoking laws (restaurants)",
  "smoking laws (workplace)",
  "smoking laws (workplace; restaurant; & bar)"
)

for (option in smoking_group1_options) {
  test_that("smoking group1 returns non-empty data frame", {
    skip_on_cran()
    result <- risk_smoking(option)
    expect_true(is.data.frame(result))
  })
}

# group2
smoking_group2_options <- c(
  "smokers (stopped for 1 day or longer)",
  "smoking not allowed at work (all people)",
  "smoking not allowed in home (all people)"
)

for (option in smoking_group2_options) {
  test_that("smoking group2 returns non-empty data frame", {
    skip_on_cran()
    result <- risk_smoking(option,
      sex = "both sexes",
      datatype = "direct estimates"
    )
    expect_true(is.data.frame(result))
  })
}

# group3
smoking_group3_options <- c(
  "smoking not allowed at work (current smokers)",
  "smoking not allowed at work (former/never smokers)",
  "smoking not allowed in home (current smokers)",
  "smoking not allowed in home (former/never smokers)"
)

for (option in smoking_group3_options) {
  test_that("smoking group3 returns non-empty data frame", {
    skip_on_cran()
    result <- risk_smoking(option,
      sex = "both sexes",
      datatype = "direct estimates"
    )
    expect_true(is.data.frame(result))
  })
}

# group4
smoking_group4_options <- c(
  "former smoker; ages 18+",
  "former smoker, quit 1 year+; ages 18+"
)

for (option in smoking_group4_options) {
  test_that("smoking group4 returns non-empty data frame", {
    skip_on_cran()
    result <- risk_smoking(option,
      sex = "both sexes",
      datatype = "county level modeled estimates", area = "ca"
    )
    expect_true(is.data.frame(result))
  })
}

# group5
smoking_group5_options <- c(
  "smokers (ever); ages 18+",
  "e-cigarette use; ages 18+"
)

for (option in smoking_group5_options) {
  test_that("smoking group5 returns non-empty data frame", {
    skip_on_cran()
    result <- risk_smoking(option,
      race = "hispanic (any race)",
      sex = "both sexes", datatype = "direct estimates"
    )
    expect_true(is.data.frame(result))
  })
}

# group6
test_that("smoking group6 returns non-empty data frame", {
  skip_on_cran()
  result <- risk_smoking("smokers (current); ages 18+",
    race = "hispanic (any race)",
    sex = "both sexes", datatype = "direct estimates"
  )
  expect_true(is.data.frame(result))
})


# risk-smoking must have 5 columns
test_that("risk-smoking has correct number of columns", {
  skip_on_cran()
  df1 <- risk_smoking("smoking laws (any)")
  df2 <- risk_smoking("smokers (stopped for 1 day or longer)",
    sex = "both sexes",
    datatype = "county level modeled estimates", area = "wa"
  )
  df3 <- risk_smoking("smoking not allowed at work (current smokers)",
    sex = "both sexes",
    datatype = "direct estimates"
  )
  expected_columns1 <- 3
  expected_columns2 <- 5
  expected_columns3 <- 6
  expect_equal(ncol(df1), expected_columns1)
  expect_equal(ncol(df2), expected_columns2)
  expect_equal(ncol(df3), expected_columns3)
})

# test error handling
test_that("risk-smoking handles invalid smoking parameters", {
  skip_on_cran()
  expect_error(
    risk_smoking("smoking laws (any)", sex = "both sexes"),
    "For this smoking type, Race, Sex, Datatype, and Area must ALL be NULL"
  )
  expect_error(
    risk_smoking("smokers (stopped for 1 day or longer)",
      sex = "both sexes",
      datatype = "county level modeled estimates"
    ),
    paste("For county level modeled estimates on this smoking type,",
          "area must NOT be null")
  )
  expect_error(
    risk_smoking("smoking not allowed at work (current smokers)",
      race = "all races (includes hispanic)", sex = "both sexes",
      datatype = "direct estimates"
    ),
    "For all sexes in this smoking type, race and area should be NULL"
  )
  expect_error(
    risk_smoking("former smoker; ages 18+",
      sex = "both sexes",
      datatype = "county level modeled estimates"
    ),
    paste("For this smoking type, Sex, Datatype,",
          "and Area must not be NULL AND Race must be NULL")
  )
  expect_error(
    risk_smoking("smokers (ever); ages 18+",
      race = "hispanic (any race)",
      sex = "both sexes"
    ),
    paste("For this smoking type, Race, Sex,",
          "and Datatype must not be NULL AND Datatype and Area must be NULL")
  )
  expect_error(
    risk_smoking("smokers (current); ages 18+",
      race = "all races (includes hispanic)",
      sex = "both sexes", area = "wa"
    ),
    "For all races for this smoking type, Datatype must not be NULL"
  )
})

# parameter
test_that("risk-smoking has correct parameters", {
  expect_error(risk_smoking())
})
