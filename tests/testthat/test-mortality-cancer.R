#' Test Mortality Cancer
#'
#' This testthat file test the mortality_cancer function
#'
# tests class and typeof output
test_that("Output data type is correct", {
  skip_on_cran()
  output <- mortality_cancer(
    "wa", "county", "all cancer sites", "black (non-hispanic)",
    "both sexes", "ages 65+", "latest 5 year average"
  )

  expect_true(inherits(output, "data.frame"))
})

# Ensures that variables are present and working on SCP
cancer_options <- c(
  "all cancer sites", "bladder", "brain & ons", "colon & rectum",
  "esophagus", "kidney & renal pelvis", "leukemia", "liver & bile duct",
  "lung & bronchus", "melanoma of the skin", "non-hodgkin lymphoma",
  "oral cavity & pharynx", "pancreas", "stomach", "thyroid"
)

for (option in cancer_options) {
  test_that("mortality cancer returns non-empty data frame", {
    skip_on_cran()
    result <- mortality_cancer(
      "wa", "county", option, "all races (includes hispanic)",
      "both sexes", "all ages", "latest 5 year average"
    )
    expect_true(is.data.frame(result))
  })
}

female_cancer_options <- c("breast (female)", "cervix", "ovary", "uterus (corpus & uterus, nos)")

for (option in female_cancer_options) {
  test_that("mortality female cancer returns non-empty data frame", {
    skip_on_cran()
    result <- mortality_cancer("wa", "county", option, "all races (includes hispanic)", "females", "ages 50+", "latest 5 year average")

    expect_true(is.data.frame(result))
  })
}

childhood_male_cancer_options <- list(
  childhood15 = mortality_cancer(
    "ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)",
    "males", "ages <20", "latest 5 year average"
  ),
  childhood20 = mortality_cancer(
    "ca", "hsa", "childhood (ages <20, all sites)", "all races (includes hispanic)",
    "males", "ages <20", "latest 5 year average"
  ),
  prostate = mortality_cancer(
    "usa", "state", "prostate", "all races (includes hispanic)",
    "males", "ages 50+", "latest 5 year average"
  )
)

for (option_name in names(childhood_male_cancer_options)) {
  test_that("mortality cancer returns non-empty data frame", {
    skip_on_cran()
    option <- childhood_male_cancer_options[[option_name]]
    expect_true(is.data.frame(option))
  })
}

# mortality_cancer must have 14 columns
test_that("mortality_cancer has correct number of columns", {
  skip_on_cran()
  df <- mortality_cancer(
    "wa", "county", "all cancer sites", "black (non-hispanic)",
    "both sexes", "ages 65+", "latest 5 year average"
  )
  expected_columns <- 14
  expect_equal(ncol(df), expected_columns)
})

# parameter
test_that("mortality_cancer has correct parameters", {
  expect_error(mortality_cancer())
})
