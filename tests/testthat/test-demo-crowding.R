# test_that("demo-crowding", {
#   test_crowding1 <- demo_crowding("WA", "hsa", "All Races (includes Hispanic)")
# 
# 
#   # object, length, dimensions,
#   # ranges -ex: greater than 10 rows
#   # errors
# 
# 
#   #expect_error() - catches an expected error
# })


test_that("Output data type is correct", {
  output <- demo_crowding("usa", "state", "All Races (includes Hispanic)")
  
  expect_equal(class(output), "data.frame",
               info = "Output should be a data frame")
  
  expect_equal(typeof(output), "list",
               info = "Output should have list storage type (since data frames are lists)")
})