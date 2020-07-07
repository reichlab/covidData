library(readr)
library(purrr)
library(tibble)
library(here)

# after this your working directory should be covidModels/R-package
setwd(here())
setwd('./R-package')


# deaths
files <- Sys.glob("data-raw/JHU/*deaths_US.csv")

jhu_deaths_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, '/'),
    function(x) substr(x[3], 1, 10)),
  data = purrr::map(
    files,
    function(filename) readr::read_csv(filename))
)

save(jhu_deaths_data, file = 'data/jhu_deaths_data.rdata')

# cases
files <- Sys.glob("data-raw/JHU/*confirmed_US.csv")

jhu_cases_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, '/'),
    function(x) substr(x[3], 1, 10)),
  data = purrr::map(
    files,
    function(filename) readr::read_csv(filename))
)

save(jhu_cases_data, file = 'data/jhu_cases_data.rdata')
