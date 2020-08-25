library(dplyr)
testthat::context("load_jhu_data-impute_and_redistribute")

# read in data
data <- covidData::load_jhu_data(
  spatial_resolution = "state",
  temporal_resolution = "daily",
  measure = "deaths", replace_negatives = TRUE,
  adjustment_cases = "none")

before_adjustment_data <- dplyr::filter(data, location == "08")

other_location_data <- dplyr::filter(data, location != "08")

data_case_a <- covidData::load_jhu_data(
  spatial_resolution = "state",
  temporal_resolution = "daily",
  measure = "deaths", replace_negatives = FALSE,
  adjustment_method = 'impute_and_redistribute',
  adjustment_cases = 'CO-2020-03-26')

after_adjustment_data_case_a <- dplyr::filter(data_case_a, location == "08")

other_location_data_case_a <- dplyr::filter(data_case_a, location != '08')

data_case_b <- covidData::load_jhu_data(
  spatial_resolution = "state",
  temporal_resolution = "daily",
  measure = "deaths", replace_negatives = FALSE,
  adjustment_method = 'impute_and_redistribute',
  adjustment_cases = 'CO-2020-04-24')

after_adjustment_data_case_b <- dplyr::filter(data_case_b, location == "08")

other_location_data_case_b <- dplyr::filter(data_case_b, location != '08')

# case a, 1
# observed is negative, replacement is imputed
test_that("negative diff, incidents are nonnegative before and on the adjustment date", {
  
  # negative obs is on 2020-03-26
  expect_true(
    all(
      after_adjustment_data_case_a[after_adjustment_data_case_a$date <= "2020-03-26", ]$inc >= 0))
})

# case a, 2
# observed is negative, replacement is imputed
test_that("negative diff, incidents are unchanged after the adjustment date", {
  expect_true(
    all(
      after_adjustment_data_case_a[after_adjustment_data_case_a$date > "2020-03-26", ]$inc ==
                    before_adjustment_data[before_adjustment_data$date > "2020-03-26", ]$inc))
})

# case a, 3
# observed is negative, replacement is imputed
test_that("negative diff, cumulative counts are unchanged on and after the adjustment date", {
  expect_true(
    all(
      after_adjustment_data_case_a[after_adjustment_data_case_a$date >= "2020-03-26", ]$cum ==
                    before_adjustment_data[before_adjustment_data$date >= "2020-03-26", ]$cum))
})


# case b, 1
# observed is positive, replacement also positive but less than observed
test_that("positive diff, incidents are nonnegative before and on the adjustment date", {
  # 2020-04-24
  
  expect_true(
    all(
      after_adjustment_data_case_b[after_adjustment_data_case_b$date <= "2020-04-24", ]$inc >= 0))
})


# case b, 2
# observed is positive, replacement also positive but less than observed
test_that("positive diff, incidents are unchanged after the adjustment date", {
  # 2020-04-24
  
  expect_true(
    all(
      after_adjustment_data_case_b[after_adjustment_data_case_b$date > "2020-04-24", ]$inc ==
                    before_adjustment_data[before_adjustment_data$date > "2020-04-24", ]$inc))
})

# case b, 3
# observed is negative, replacement is imputed
test_that("negative diff, cumulative counts are unchanged on and after the adjustment date", {
  expect_true(all(
    after_adjustment_data_case_b[after_adjustment_data_case_b$date >= "2020-04-24", ]$cum ==
                    before_adjustment_data[before_adjustment_data$date >= "2020-04-24", ]$cum))
})

test_that("data should be the same for locations other than the adjustment location",{
  
  expect_true(all(other_location_data == other_location_data_case_a))
  
  expect_true(all(other_location_data == other_location_data_case_b))
})
