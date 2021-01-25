library(readr)
library(purrr)
library(tibble)
library(here)

# after this your working directory should be covidData
setwd(here())

# load preprocessing functions in R/calc_jhu_inc.R
# they are defined there so they can be unit tested, but loaded manually here
# so that data can be processed if the covidData package is not installed yet.
source("R/calc_jhu_inc.R")

# deaths
files <- Sys.glob("data-raw/JHU/*deaths_US.csv")

jhu_deaths_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, "/"),
    function(x) substr(x[3], 1, 10)
  ),
  data = purrr::map(
    files,
    function(filename) suppressMessages(readr::read_csv(filename) %>% 
                                          calc_jhu_inc())
  )
)

save(jhu_deaths_data, file = "data/jhu_deaths_data.rdata")

# cases
files <- Sys.glob("data-raw/JHU/*confirmed_US.csv")

jhu_cases_data <- tibble::tibble(
  issue_date = purrr::map_chr(
    strsplit(files, "/"),
    function(x) substr(x[3], 1, 10)
  ),
  data = purrr::map(
    files,
    function(filename) suppressMessages(readr::read_csv(filename) %>% 
                                          calc_jhu_inc())
  )
)

save(jhu_cases_data, file = "data/jhu_cases_data.rdata")
