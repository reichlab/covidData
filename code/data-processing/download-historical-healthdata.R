library(tidyverse)
library(xml2)
library(rvest)
library(here)

# after this your working directory should be covidData
setwd(here())

#' utility function to find the link to a time series csv file in its metadata
#' page
get_file_link <- function(link) {
  message(link)
  target_html <- xml2::read_html(link)

  # this pattern works for the time series files
  result <- target_html %>%
    xml2::xml_find_all("//div[contains(@class, 'download')]") %>%
    html_children() %>%
    html_attr("href")

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
  revisions_html <- xml2::read_html(revisions_page)

  all_revisions_pages <- revisions_page

  # check to see if there are enough revisions that they have been paginated
  # if so, add in links to other pages
  pages <- revisions_html %>%
    xml2::xml_find_all("//ul[contains(@class, 'pagination pager')]") %>%
    xml2::xml_find_all("//li[contains(@class, 'pager-item')]") %>%
    html_children() %>%
    html_attr("href") %>%
    (function(rel_link) { paste0("https://healthdata.gov", rel_link) })

  if (length(pages) > 0) {
    all_revisions_pages <- c(all_revisions_pages, pages)
  }

  results <- purrr::map_dfr(all_revisions_pages, get_revisions_metadata_one_page)

  # one-off manual adjustment for daily data
  if (revisions_page == "https://healthdata.gov/node/3281086/revisions") {
    results$file_link[results$issue_date == "2020-11-03"] <-
      "https://healthdata.gov/sites/default/files/reported_hospital_utilization_20201103_2139.csv"
  }

  return(results)
}

#' utility function to get metadata about available revisions
get_revisions_metadata_one_page <- function(revisions_page) {
  # load html for page listing revisions
  revisions_html <- xml2::read_html(revisions_page)

  # extract table rows with links to revised data
  # note the first row is just a table header
  revisions_nodes <- revisions_html %>%
    rvest::html_nodes('tr')
  revisions_nodes <-
    revisions_nodes[2:length(revisions_nodes)]

  # extract info about revision files: link to page, link to file, issue date
  revisions_meta <- data.frame(
    link = purrr::map_chr(
      revisions_nodes,
      function(node) {
        node %>%
          html_children() %>%
          `[[`(1) %>%
          html_children() %>%
          `[[`(1) %>%
          html_attr("href") %>%
          (function(rel_link) { paste0("https://healthdata.gov", rel_link) })
      }
    )
  ) %>%
    dplyr::mutate(
      file_link = purrr::map_chr(
        link,
        get_file_link
      )
    )

  revisions_meta <- revisions_meta %>%
    dplyr::mutate(
      issue_date = file_link %>%
        substr(nchar(file_link) - 16, nchar(file_link) - 9) %>%
        lubridate::ymd()
    )

  return(revisions_meta)
}

# get issue date and link to file for each data revision
timeseries_revisions_meta <-
  get_revisions_metadata("https://healthdata.gov/node/3565481/revisions")

daily_revisions_meta <-
  get_revisions_metadata("https://healthdata.gov/node/3281086/revisions")

# filter to issue dates at least as large as Nov. 15, 2020
# This is the first date that the previous_day_admission_adult_covid_confirmed
# and previous_day_admission_pediatric_covid_confirmed fields are recorded
# in the time series data files
timeseries_revisions_meta <- timeseries_revisions_meta %>%
  dplyr::filter(issue_date >= "2020-11-15")

daily_revisions_meta <- daily_revisions_meta %>%
  dplyr::filter(issue_date >= "2020-11-15")

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
      timeseries_revisions_meta$file_link[i],
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
      "_daily_covid19_hospitalizations_US.csv")
  )

  if (!file.exists(destination_path)) {
    data <- suppressMessages(readr::read_csv(
      daily_revisions_meta$file_link[i],
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
