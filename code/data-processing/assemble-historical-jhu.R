library(readr)
library(purrr)
library(tibble)
library(here)
library(covidData)

# after this your working directory should be covidData
setwd(here())

# US deaths
files <- Sys.glob("data-raw/JHU/*deaths_US.csv")

jhu_us_deaths_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, "/"),
    function(x) substr(x[3], 1, 10)),
  data = purrr::map(
    files,
    function(filename) suppressMessages(readr::read_csv(filename)))
)

save(jhu_us_deaths_data, file = "data/jhu_us_deaths_data.rdata")

# US cases
files <- Sys.glob("data-raw/JHU/*confirmed_US.csv")

jhu_us_cases_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, "/"),
    function(x) substr(x[3], 1, 10)),
  data = purrr::map(
    files,
    function(filename) suppressMessages(readr::read_csv(filename)))
)

save(jhu_us_cases_data, file = "data/jhu_us_cases_data.rdata")

# Global deaths
files <- Sys.glob("data-raw/JHU/*deaths_global.csv")

jhu_global_deaths_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, "/"),
    function(x) substr(x[3], 1, 10)),
  data = purrr::map(
    files,
    function(filename) suppressMessages(readr::read_csv(filename) %>%
                                          calc_jhu_global_cum()))
)

save(jhu_global_deaths_data, file = "data/jhu_global_deaths_data.rdata")

# Global cases
files <- Sys.glob("data-raw/JHU/*confirmed_global.csv")

jhu_global_cases_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, "/"),
    function(x) substr(x[3], 1, 10)),
  data = purrr::map(
    files,
    function(filename) suppressMessages(readr::read_csv(filename) %>%
                                          calc_jhu_global_cum()))
)

save(jhu_global_cases_data, file = "data/jhu_global_cases_data.rdata")
