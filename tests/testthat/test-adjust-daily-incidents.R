library(dplyr)
library(testthat)

# read in data
data = load_jhu_data(spatial_resolution = "state", 
                     temporal_resolution = "daily",
                     measure = "death", replace_negatives = FALSE, 
                     adjustment_cases = 'none')%>%
  # NY state
  dplyr:: filter(location == '36')

seed = 1234

#negative obs is on 2020-04-19
new_inc_case_a = adjust_daily_incidence(data, 'NY-2020-04-19', seed)

new_inc_case_b = adjust_daily_incidence(data, 'NY-2020-06-12', seed)

# case a, 1  
# observed is negative, replacement is imputed
test_that("negative diff, incidents are nonnegative before and on the adjustment date"{
  
  #negative obs is on 2020-04-19
  expect_true(all(new_inc_case_a[data$date <= "2020-04-19"] >= 0))
  
})

# case a, 2
# observed is negative, replacement is imputed 
test_that("negative diff, incidents are unchanged after the adjustment date"{
  
  expect_true(all(new_inc_case_a[data$date >= "2020-04-19"] == 
                    data[data$date >="2020-04-19", ]$inc))
  
})


# case b, 1 
# observed is positive, replacement also positive but less than observed
test_that("positive diff, incidents are nonnegative before and on the adjustment date"{
  # 2020-06-12
  
  expect_true(all(new_inc_case_b[data$date <= "2020-06-12"] >= 0))
  
})


# case b, 2 
# observed is positive, replacement also positive but less than observed
test_that("positive diff, incidents are unchanged after the adjustment date"{
  # 2020-06-12
  
  expect_true(all(new_inc_case_b[data$date >= "2020-06-12"] == 
                    data[data$date >="2020-06-12", ]$inc))
  
})
