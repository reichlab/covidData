library(dplyr)
library(testthat)

# read csv file
jhu_data = readr::read_csv("test data/2020-07-16_time_series_covid19_confirmed_US.csv")

test_that("agreement with daily cumulative counts in csv file
          for all locations other than colorado",{
  
  states_to_keep <- c(
    'Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California',
    'Colorado', 'Connecticut', 'Delaware', 'District of Columbia',
    'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois',
    'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine',
    'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',
    'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada',
    'New Hampshire', 'New Jersey', 'New Mexico', 'New York',
    'North Carolina', 'North Dakota', 'Northern Mariana Islands',
    'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico',
    'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
    'Texas', 'Utah', 'Vermont', 'Virgin Islands', 'Virginia',
    'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
  
  state_results <- jhu_data %>%
    tidyr::pivot_longer(
      matches('^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$'),
      names_to = 'date',
      values_to = 'cum') %>%
    dplyr::mutate(
      date = as.character(lubridate::mdy(date))) %>%
    dplyr::filter(Province_State %in% states_to_keep) %>%
    dplyr::mutate(location_name = Province_State) %>%
    dplyr::group_by(location_name, date) %>%
    dplyr::summarize(cum = sum(cum)) %>%
    dplyr::group_by(location_name) %>%
    dplyr::mutate(inc = diff(c(0,cum))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(covidData::fips_codes, by = 'location_name') %>%
    dplyr::select(location, date, cum, inc)%>%
    dplyr::mutate(date = as.Date(date))
  
  case1 = covidData::load_jhu_data(
  issue_date = '2020-07-16',
  temporal_resolution = 'daily',
  adjustment_cases = 'CO-2020-04-24',
  adjustment_method = 'fill_na',
  measure = 'cases') 
  
  case1 = case1 %>% dplyr::select(-inc) %>% dplyr::filter(location != '08')
  state_results_case1 = state_results %>% dplyr::select(-inc) %>% dplyr::filter(location != '08')
  

  expect_equal(case1,state_results_case1)
})


test_that("agreement in daily incident and cumulative counts between 
a call to function with adjustment_cases = 'none'and adjustment_cases = 'CO-2020-04-24'
          for all locations other than colorado",{ 
 
  no_adjustments = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    adjustment_cases = 'none',
    measure = 'cases') %>%
    dplyr::filter(location != '08')
    
  
  with_adjustments = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    adjustment_cases = 'CO-2020-04-24',
    adjustment_method = 'fill_na',
    measure = 'cases') %>%
    dplyr::filter(location != '08')
  
  expect_equal(no_adjustments,with_adjustments)
  
  })

test_that("daily incident count for CO on 2020-04-24 to be NA
  and cumulative counts on or after 2020-04-24 to be NA",{

  after_adjustments = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    adjustment_cases = 'CO-2020-04-24',
    adjustment_method = 'fill_na',
    measure = 'cases') %>%
    dplyr::filter(location == '08')
  
  no_adjustments = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    adjustment_cases = 'none',
    measure = 'cases') %>%
    dplyr::filter(location == '08')
  
  

  # make sure Colorado inc only changes on '2020-04-24'
  expect_true(is.na(after_adjustments$inc[after_adjustments$date == '2020-04-24']))
  expect_equal(after_adjustments$inc[after_adjustments$date != '2020-04-24'], 
               no_adjustments$inc[no_adjustments$date != '2020-04-24'])
  

  # makes sure Colorado cum only changes for all dates >= '2020-04-24
  expect_true(all(is.na(after_adjustments$cum[after_adjustments$date >= '2020-04-24'])))
  expect_equal(after_adjustments$inc[after_adjustments$date < '2020-04-24'], 
               no_adjustments$inc[no_adjustments$date < '2020-04-24'])
  
})

test_that("agreement with daily cumulative counts in csv file 
          for all locations other than counties in colorado",{
  
  county_results <- jhu_data %>%
    tidyr::pivot_longer(
      matches('^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$'),
      names_to = 'date',
      values_to = 'cum') %>%
    dplyr::mutate(
      date = as.character(lubridate::mdy(date))) %>%
    dplyr::filter(FIPS > 100) %>%
    dplyr::mutate(
      location = sprintf("%05d", FIPS)) %>%
    dplyr::filter(location < '80001') %>%
    dplyr::group_by(location) %>%
    dplyr::mutate(inc = diff(c(0,cum))) %>%
    dplyr::select(location, date, cum, inc) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(date = as.Date(date))
  
  case1 = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    spatial_resolution = 'county',
    adjustment_cases = 'CO-2020-04-24',
    adjustment_method = 'fill_na',
    measure = 'cases') 
  
  case1 = case1 %>% dplyr::select(-inc) %>% dplyr::filter(stringr::str_sub(location, start = 1, end=2) != '08')
  county_results_case1 = county_results %>% dplyr::select(-inc) %>% dplyr::filter(stringr::str_sub(location, start = 1, end=2) != '08')
 
  expect_equal(case1,county_results_case1)
})


test_that("agreement in daily incident and cumulative counts between 
a call to function with adjustment_cases = 'none' and adjustment_cases = 'CO-2020-04-24' 
          for all locations other than counties in colorado",{ 
  no_adjustments =covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    spatial_resolution = 'county',
    adjustment_cases = 'none',
    measure = 'cases') %>%
    dplyr::filter(stringr::str_sub(location, start = 1, end=2) != '08')
  
  
  with_adjustments = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    spatial_resolution = 'county',
    adjustment_cases = 'CO-2020-04-24',
    adjustment_method = 'fill_na',
    measure = 'cases') %>%
    dplyr::filter(stringr::str_sub(location, start = 1, end=2) != '08')
  
  expect_equal(no_adjustments,with_adjustments)
  
})

test_that("daily incident count for counties in CO on 2020-04-24 to be NA
  and cumulative counts on or after 2020-04-24 to be NA.",{
    
  after_adjustments = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    spatial_resolution = 'county',
    adjustment_cases = 'CO-2020-04-24',
    adjustment_method = 'fill_na',
    measure = 'cases') %>%
    dplyr::filter(stringr::str_sub(location, start = 1, end=2)  == '08')
  
  no_adjustments = covidData::load_jhu_data(
    issue_date = '2020-07-16',
    temporal_resolution = 'daily',
    spatial_resolution = 'county',
    adjustment_cases = 'none',
    measure = 'cases') %>%
    dplyr::filter(stringr::str_sub(location, start = 1, end=2) == '08')
  
  # make sure all county-level inc in Colorado only changes on '2020-04-24'
  expect_true(all(is.na(after_adjustments$inc[after_adjustments$date == '2020-04-24'])))
  expect_equal(after_adjustments$inc[after_adjustments$date != '2020-04-24'], 
               no_adjustments$inc[no_adjustments$date != '2020-04-24'])
  
  # makes sure all county-level cum in Colorado only changes for all dates >= '2020-04-24
  expect_true(all(is.na(after_adjustments$cum[after_adjustments$date >= '2020-04-24'])))
  expect_equal(after_adjustments$inc[after_adjustments$date < '2020-04-24'], 
               no_adjustments$inc[no_adjustments$date < '2020-04-24'])
  
})


