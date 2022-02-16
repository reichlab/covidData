#' Retrieve hosp time series data update history from healthdata.gov
#' 
#' @return a data.frame with columns issue_date, issue_datetime and file_link 
healthdata_timeseries_history <- function(){
  # get issue date and link to file for each timeseries data revision
  # over the weekend of 2020-03-13 to 2021-03-14, healthdata.gov changed their
  # data storage mechanism.  For simplicity, we save and re-use the older files,
  # downloading only files released after that time
  temp <- httr::GET(
    "https://healthdata.gov/resource/qqte-vkut.json",
    config = httr::config(ssl_verifypeer = FALSE)) %>%
    as.character() %>%
    jsonlite::fromJSON()
  
  timeseries_revisions_meta <- data.frame(
    issue_date = lubridate::ymd(substr(temp$update_date, 1, 10)), # actually the file creation date, not the issue date
    issue_datetime = temp$update_date,
    file_link = temp$archive_link$url,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(
      !(substr(file_link, nchar(file_link) - 3, nchar(file_link)) == "zip"),
      issue_date > "2021-03-12" # last date we have archived from before udpates
    ) %>%
    dplyr::group_by(issue_date) %>%
    dplyr::slice_max(issue_datetime)
  
  return (timeseries_revisions_meta)
}

#' Download time series data at a specific issue date
#' @param issue_date character issue date (i.e. report date) in format 'yyyy-mm-dd'
#' @param healthdata_timeseries_history a data.frame with hosp time series data update history
#'
#' @return data.frame with columns state, date, previous_day_admission_adult_covid_confirmed 
#' and previous_day_admission_pediatric_covid_confirmed
download_healthdata_timeseries <- function(issue_date, healthdata_timeseries_history){
  healthdata_timeseries_history <- dplyr::filter(healthdata_timeseries_history, issue_date == UQ(issue_date))
  data <- suppressMessages(
    readr::read_csv(
      healthdata_timeseries_history$file_link %>%
        httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>%
        httr::content(as = "text"),
      col_types = readr::cols_only(
        state = readr::col_character(),
        date = readr::col_character(), 
        previous_day_admission_adult_covid_confirmed = readr::col_integer(),
        previous_day_admission_pediatric_covid_confirmed = readr::col_integer()
      )
    ) %>%
      dplyr::mutate(
        date = lubridate::ymd(substr(date, 1, 10))
      )
  )
  return (data)
}

#' Retrieve hosp daily revision data update history from healthdata.gov
#' 
#' @return a data.frame with columns issue_date, date, issue_datetime and file_link
healthdata_dailyrevision_history <- function(){  
  # get issue date and link to file for each daily data revision
  # over the weekend of 2020-03-13 to 2021-03-14, healthdata.gov changed their
  # data storage mechanism.  For simplicity, we save and re-use the older files,
  # downloading only files released after that time
  
  # temporarily commenting out collection of new daily updates --
  # the resource is not available to the public as of 2021-03-29
  temp <- httr::GET(
    "https://healthdata.gov/resource/4cnb-m4rz.json",
    config = httr::config(ssl_verifypeer = FALSE)) %>%
    as.character() %>%
    jsonlite::fromJSON()
  
  daily_revisions_meta <- data.frame(
    issue_date = lubridate::ymd(substr(temp$update_date, 1, 10)), # actually the file creation date, not the issue date
    date = lubridate::ymd(substr(temp$update_date, 1, 10)),
    issue_datetime = temp$update_date,
    file_link = temp$archive_link$url,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(
      !(substr(file_link, nchar(file_link) - 3, nchar(file_link)) == "zip"),
      issue_date > "2021-03-12" # last date we have archived from before udpates
    ) %>%
    dplyr::group_by(issue_date) %>%
    dplyr::slice_max(issue_datetime) 
  
  # 4 cols: issue_date, date, issue_datetime, file_link
  # date is date column in downloaded csv
  return (daily_revisions_meta)
} 

#' Download daily revision data at a specific issue date
#' @param issue_date character issue date (i.e. report date) in format 'yyyy-mm-dd'
#' @param healthdata_dailyrevision_history a data.frame with hosp time series data update history
#'
#' @return data.frame with columns state, date, previous_day_admission_adult_covid_confirmed 
#' and previous_day_admission_pediatric_covid_confirmed
download_healthdata_dailyrevision <- function(issue_date, healthdata_dailyrevision_history){
  healthdata_dailyrevision_history <- dplyr::filter(healthdata_dailyrevision_history, issue_date == UQ(issue_date))
  data <- suppressMessages(
    readr::read_csv(
      healthdata_dailyrevision_history$file_link %>%
        httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
        httr::content(as = "text"),
      col_types = readr::cols_only(
        state = readr::col_character(),
        previous_day_admission_adult_covid_confirmed = readr::col_integer(),
        previous_day_admission_pediatric_covid_confirmed = readr::col_integer()
      )
    ) %>%
      dplyr::mutate(date = healthdata_dailyrevision_history$date)
  )
  
  return(data)
  
}

#' Construct a merged healthdata data set for a single issue date by pulling
#' from a timeseries update if one is available, or by augmenting the last time
#' series update on or before the issue date with daily updates that were made
#' after the last time series update and on or before the issue date
#' 
#' @param issue_date date object for issue date 
#' @param healthdata_timeseries_history data.frame of issue_date and data reported that
#' date
#' @param healthdata_dailyrevision_history data.frame of issue_date, date, and data
#' reported
#' 
#' @return tibble of issue_date and data reported on or by that date
build_healthdata_data <- function(
  issue_date,
  healthdata_timeseries_history,
  healthdata_dailyrevision_history) {
  healthdata_hosp_early_data <- covidData::healthdata_hosp_early_data
  # case 1: issue_date is a date before "2021-03-12"
  if (issue_date %in% healthdata_hosp_early_data$issue_date){
    result <- healthdata_hosp_early_data %>%
      dplyr::filter(issue_date == UQ(issue_date))
  } 
  # case 2: issue_date is a date with up-to-date time series data
  else if (issue_date %in% healthdata_timeseries_history$issue_date) {
    data <- download_healthdata_timeseries(issue_date, healthdata_timeseries_history)
    result <- tibble::tibble(
      issue_date = issue_date,
      data = list(data))
    
  } 
  # case 3: issue_date is a date with old time series data and up-to-date daily revision data
  else if (issue_date %in% healthdata_dailyrevision_history$issue_date) {
    healthdata_dailyrevision_history <- healthdata_dailyrevision_history %>%
      dplyr::filter(issue_date <= UQ(issue_date))
    
    # get the latest date that is smaller than the specified issue_date
    last_weekly_date <- max(healthdata_timeseries_history$issue_date[healthdata_timeseries_history$issue_date <= issue_date])
    
    last_weekly_data <- download_healthdata_timeseries(last_weekly_date, healthdata_timeseries_history)
    last_date <- lubridate::ymd(max(last_weekly_data$date))
    max_date <- lubridate::ymd(max(healthdata_dailyrevision_history$date))
    num_dates_to_add <- max_date - last_date
    
    for (i in seq_len(num_dates_to_add)) {
      new_date <- last_date + i
      if (new_date %in% healthdata_dailyrevision_history$date) {
        healthdata_dailyrevision_issue_date <- healthdata_dailyrevision_history %>%
          dplyr::filter(date == new_date) %>%
          dplyr::slice_max(issue_date) %>%
          dplyr::pull(issue_date)
        
        # can reduce here change paramter to be a row of df
        healthdata_dailyrevision <- download_healthdata_dailyrevision(
          healthdata_dailyrevision_issue_date,
          healthdata_dailyrevision_history
          ) %>% 
          dplyr::mutate(date = new_date)
        
        # if daily data for new_date is available, append it by pulling
        # from the largest issue_date for that day.
        last_weekly_data <- dplyr::bind_rows(
          last_weekly_data,
          healthdata_dailyrevision
        )
        
      } else {
        required_locations <- unique(last_weekly_data$state)
        last_weekly_data <- dplyr::bind_rows(
          last_weekly_data,
          tidyr::expand_grid(
            state = required_locations,
            date = new_date,
            previous_day_admission_adult_covid_confirmed = NA_real_,
            previous_day_admission_pediatric_covid_confirmed = NA_real_
          )
        )
      }
    }
    
    result <- tibble::tibble(
      issue_date = issue_date,
      data = list(last_weekly_data))
  } 
  # case 4: neither time series nor daily data were released on this issue date
  else {
    result <- NULL
  }
  
  return(result)
}

