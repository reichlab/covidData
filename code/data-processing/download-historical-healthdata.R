library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(jsonlite)
library(here)

# after this your working directory should be covidData
setwd(here())

#' utility function to find the link to a time series csv file in its metadata
#' page
get_file_link <- function(link) {
  message(link)
  target_html <- xml2::read_html(httr::GET(link, config = httr::config(ssl_verifypeer = FALSE)))

  # this pattern works if the link was to an html page
  result <- target_html %>%
    xml2::xml_find_all("//div[contains(@class, 'download')]") %>%
    html_children() %>%
    html_attr("href")
  
  # otherwise, if it was to a json page, extract the correct url and try again


  # if (length(result) == 0) {
  #   # this pattern works for the daily files
  #   result <- target_html %>%
  #     xml2::xml_find_all("//span[contains(@class, 'links')]") %>%
  #     xml2::xml_find_all("//a[contains(@class, 'data-link')]") %>%
  #     html_attr("href")
  # }

  return(result)
}

#' get revision date and link to actual data file from a potentially-paginated
#' html page with a table listing revisions
get_revisions_metadata <- function(revisions_page) {
  # load html for page listing revisions
  revisions_html <- xml2::read_html(httr::GET(revisions_page, config = httr::config(ssl_verifypeer = FALSE)))

  all_revisions_pages <- revisions_page

  # check to see if there are enough revisions that they have been paginated
  # if so, add in links to other pages
  pages <- revisions_html %>%
    xml2::xml_find_all("//ul[contains(@class, 'pagination pager')]") %>%
    xml2::xml_find_all("//li[contains(@class, 'pager-item')]") %>%
    html_children() %>%
    html_attr("href")

  if (length(pages) > 0) {
    all_revisions_pages <- c(
      all_revisions_pages,
      paste0("http://healthdata.gov", pages)
    )
  }

  results <- purrr::map_dfr(all_revisions_pages, get_revisions_metadata_one_page)

  # one-off manual adjustment for daily data
  if (revisions_page == "http://healthdata.gov/node/3281086/revisions") {
    results$file_link[results$issue_date == "2020-11-03"] <-
      "http://healthdata.gov/sites/default/files/reported_hospital_utilization_20201103_2139.csv"
  }

  return(results)
}

#' utility function to get metadata about available revisions
get_revisions_metadata_one_page <- function(revisions_page) {
  # load html for page listing revisions
  revisions_html <- xml2::read_html(httr::GET(revisions_page, config = httr::config(ssl_verifypeer = FALSE)))

  # extract table rows with links to revised data
  # note the first row is just a table header
  revisions_nodes <- revisions_html %>%
    rvest::html_nodes('tr')

  revisions_nodes <-
    revisions_nodes[2:length(revisions_nodes)]

  # extract info about revision files: link to file, issue date
  revisions_meta <- purrr::map_dfr(
    revisions_nodes,
    function(node) {
      link <- node %>%
        html_children() %>%
        `[[`(1) %>%
        html_children() %>%
        `[[`(1) %>%
        html_attr("href") %>%
        (function(rel_link) { paste0("http://healthdata.gov", rel_link) })
      
      file_link <- get_file_link(link)
      if (length(file_link) == 0) {
        # maybe we can still find the file...
        if (grepl("revision-published", node %>% html_attr("class"))) {
          # link to currently published data file sometimes fails;
          # we can access it by manually constructing the link
          json_description <- httr::GET(link, config = httr::config(ssl_verifypeer = FALSE)) %>%
            as.character() %>%
            jsonlite::fromJSON()
          new_link <- paste0(
            "http://healthdata.gov/node/",
            json_description$nid,
            "/revisions/",
            json_description$vid,
            "/view"
          )
          file_link <- get_file_link(new_link)
        }
      }
      
      # if we still couldn't find the file, issue a warning and return NULL
      if (length(file_link) == 0) {
        warning(paste0("Unable to download file."))
        return(NULL)
      }

      # for daily files, date in file name is the date targeted by the file
      date_start_ind <- regexpr(
        "(202\\d\\d\\d\\d\\d)",
        file_link
      )
      date <- substr(file_link, date_start_ind, date_start_ind + 7) %>%
        lubridate::ymd()
      
      # issue date is the date of release, in the link text
      issue_date <- node %>%
        html_children() %>%
        `[[`(1) %>%
        html_children() %>%
        `[[`(1) %>%
        html_text() %>%
        substr(6, 15) %>%
        lubridate::mdy()
      
      return(data.frame(
        date = date,
        issue_date = issue_date,
        file_link = file_link,
        stringsAsFactors = FALSE
      ))
    }
  )

  return(revisions_meta)
}

# get issue date and link to file for each data revision
timeseries_revisions_meta <-
  get_revisions_metadata("http://healthdata.gov/node/3565481/revisions") %>%
  # filter to issue dates at least as large as Nov. 15, 2020
  # This is the first date that the previous_day_admission_adult_covid_confirmed
  # and previous_day_admission_pediatric_covid_confirmed fields are recorded
  # in the time series data files
  dplyr::filter(date >= "2020-11-15") %>%
  # if there were multiple issues on the same date for the same file date,
  # keep only the last one
  dplyr::group_by(date, issue_date) %>%
  dplyr::slice_head(n = 1) %>%
  # if there were multiple issues for the same file, keep only the first
  # issue date for that file
  dplyr::group_by(file_link) %>%
  dplyr::slice_min(issue_date) %>%
  # issue dates are sometimes recorded incorrectly on the website
  # set to date if reported issue date is less than date
  dplyr::mutate(
    issue_date = pmax(issue_date, date)
  ) %>%
  as.data.frame()

daily_revisions_meta <-
  get_revisions_metadata("http://healthdata.gov/node/3281086/revisions") %>%
  # filter to issue dates at least as large as Nov. 15, 2020
  # This is the first date that the previous_day_admission_adult_covid_confirmed
  # and previous_day_admission_pediatric_covid_confirmed fields are recorded
  # in the time series data files
  dplyr::filter(date >= "2020-11-15") %>%
  # if there were multiple issues on the same date for the same file date,
  # keep only the last one
  dplyr::group_by(date, issue_date) %>%
  dplyr::slice_head(n = 1) %>%
  # if there were multiple issues for the same file, keep only the first
  # issue date for that file
  dplyr::group_by(file_link) %>%
  dplyr::slice_min(issue_date) %>%
  # issue dates are sometimes recorded incorrectly on the website
  # set to date if reported issue date is less than date
  dplyr::mutate(
    issue_date = pmax(issue_date, date)
  ) %>%
  as.data.frame()

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
    data <- suppressMessages(readr::read_csv(
      timeseries_revisions_meta$file_link[i] %>%
        httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
        content(as = "text"),
      col_types = cols_only(
        state = col_character(),
        date = col_date(format = "%Y-%m-%d"),
        previous_day_admission_adult_covid_confirmed = col_integer(),
        previous_day_admission_pediatric_covid_confirmed = col_integer()
      )
    ))

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
