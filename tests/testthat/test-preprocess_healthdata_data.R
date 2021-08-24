library(dplyr)
library(readr)

testthat::context("preprocess_healthdata_data")

test_that("preprocess_healthdata_data correct output",{
  daily_data <- readr::read_csv("test data/test.csv")
  daily_data <- daily_data %>% dplyr::mutate(date = lubridate::mdy(date))
  fips <- covidData::fips_codes
  healthdata_hosp_data_daily <- tibble::tibble(
    issue_date = "2020-11-16",
    date = "2020-11-15",
    data = list(daily_data)
  )
  actual_result <- covidData:::preprocess_healthdata_data(healthdata_hosp_data_daily, fips)
  expected_result <- read_csv("test data/test_result.csv")
  expected_result <- expected_result %>% 
                     dplyr::mutate(date = lubridate::mdy(date)) %>%
                     as.data.frame() 
  attr(expected_result, "spec") <- NULL
  expect_equal(as.data.frame(actual_result$data[[1]]), as.data.frame(expected_result))
})



