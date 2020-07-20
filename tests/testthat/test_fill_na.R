library(dplyr)
library(testthat)
test_that("case3",{
  test = load_jhu_data(
  temporal_resolution = 'daily',
  adjustment_cases = 'CO-2020-04-24',
  adjustment_method = 'fill_na') 
  
  
  test_no_cases = load_jhu_data(
    temporal_resolution = 'daily') 
  
  expect_true(nrow(test)<nrow(test_no_cases))
})