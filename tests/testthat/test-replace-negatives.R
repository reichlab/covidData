library(dplyr)
testthat::context("replace_negatives")

# data with negative incidence
data <- covidData::load_jhu_data(spatial_resolution = 'state', 
                     temporal_resolution = 'daily',
                     measure = 'deaths', replace_negatives = FALSE, 
                     adjustment_cases = 'none') %>%
  # CO
  dplyr:: filter(location == '08')



imputed_data <- covidData::replace_negatives(data, measure = 'deaths')

# data without negative incidence
non_neg_no_replace <- covidData::load_jhu_data(spatial_resolution = 'state', 
                                    temporal_resolution = 'daily',
                                    measure = 'deaths', replace_negatives = FALSE, 
                                    adjustment_cases = 'none') %>%
  # AK
  dplyr:: filter(location == '02')

non_neg_replace <- covidData::load_jhu_data(spatial_resolution = 'state', 
                                               temporal_resolution = 'daily',
                                               measure = 'deaths', replace_negatives = TRUE, 
                                               adjustment_cases = 'none') %>%
  # AK
  dplyr:: filter(location == '02')

test_that("incidents after the last negative observation shouldn't change", {
  # find  observations with negative inc
  adjustments <- covidData::get_negative_cases(data)

  # take the last negative case
  date <- adjustments[-1, ]$date

  expect_true(all(data[data$date > date, ]$inc ==
                    imputed_data[imputed_data$date > date, ]$inc))

})

test_that("all incidents are non-negative", {
 expect_true(all(imputed_data$inc >= 0))
})

test_that("no negative incidence to replace",{
  expect_true(all(non_neg_replace == non_neg_no_replace))
})
