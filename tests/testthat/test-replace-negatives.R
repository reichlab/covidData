library(dplyr)
library(testthat)

# read in data
data = load_jhu_data(spatial_resolution = "state", 
                     temporal_resolution = "daily",
                     measure = "death", replace_negatives = FALSE, 
                     adjustment_cases = 'none')%>%
  # CO
  dplyr:: filter(location == '08')

imputed_data = replace_negatives(data)

test_that("incidents after the last negative observation shouldn't change"{
  # find  observations with negative inc
  adjustments = get_negative_cases(data)
  
  # take the last negative case 
  date = adjustments[-1,]$dates
  
  
  expect_true(all(data[data$date > date,]$inc == 
                    imputed_data[imputed_data$date > date,]$inc))
  
})


test_that("all incidents are non-negative"{
  expect_true(all(imputed_data$inc > 0))
})