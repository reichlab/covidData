library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(jsonlite)
library(here)

# after this your working directory should be covidData
setwd(here())

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
    issue_date > "2021-03-07" # last date we have archived from before udpates
  ) %>%
  dplyr::group_by(issue_date) %>%
  dplyr::slice_max(issue_datetime)

# get issue date and link to file for each daily data revision
# over the weekend of 2020-03-13 to 2021-03-14, healthdata.gov changed their
# data storage mechanism.  For simplicity, we save and re-use the older files,
# downloading only files released after that time
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


# download the actual csvs
for (path in c("./data-raw/healthdata/",
               "./data-raw/healthdata/timeseries",
               "./data-raw/healthdata/daily")) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}

for (i in seq_len(nrow(timeseries_revisions_meta))) {
  destination_path <- file.path(
    "./data-raw/healthdata/timeseries",
    paste0(timeseries_revisions_meta$issue_date[i],
      "_time_series_covid19_hospitalizations_US.csv")
  )

  if (!file.exists(destination_path)) {
    data <- suppressMessages(
      readr::read_csv(
        timeseries_revisions_meta$file_link[i] %>%
          httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>%
          content(as = "text"),
          col_types = cols_only(
            state = col_character(),
            date = col_character(), #format = "%Y/%m/%d"),
            previous_day_admission_adult_covid_confirmed = col_integer(),
            previous_day_admission_pediatric_covid_confirmed = col_integer()
          )
      ) %>%
      dplyr::mutate(
        date = lubridate::ymd(substr(date, 1, 10))
      )
    )

    readr::write_csv(data, destination_path)
  }
}

for (i in seq_len(nrow(daily_revisions_meta))) {
  destination_path <- file.path(
    "./data-raw/healthdata/daily",
    paste0(daily_revisions_meta$issue_date[i],
      "_", daily_revisions_meta$date[i],
      "_daily_covid19_hospitalizations_US.csv")
  )

  if (!file.exists(destination_path)) {
    data <- suppressMessages(
      readr::read_csv(
        daily_revisions_meta$file_link[i] %>%
          httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
          content(as = "text"),
        col_types = cols_only(
          state = col_character(),
          previous_day_admission_adult_covid_confirmed = col_integer(),
          previous_day_admission_pediatric_covid_confirmed = col_integer()
        )
      ) %>%
        dplyr::mutate(date = daily_revisions_meta$date[i])
    )

    readr::write_csv(data, destination_path)
  }
}
