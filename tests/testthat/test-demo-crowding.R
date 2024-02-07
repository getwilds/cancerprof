test_that("demo-crowding", {
  test_crowding1 <- demo_crowding("WA", "hsa", "All Races (includes Hispanic)")
  
  
  # object, length, dimensions, 
  # ranges -ex: greater than 10 rows
  # errors 
  # 
  expect_type(test_crowding1, "list")
  
  
  #expect_error() - catches an expected error
})
