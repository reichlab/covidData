library(httr)
library(dplyr)
library(here)
library(covidData)

setwd(here())
source("R/get_time_series_data_link.R")

# parse argument for download_recent parameter in get_time_series_data_link
args <- (commandArgs(TRUE))
download_recent <- args[1]

jhu_deaths_data_links <- get_time_series_data_link(measure = "deaths", 
                                                   download_files = TRUE,
                                                   download_recent = download_recent)
save(jhu_deaths_data_links, file = "data/jhu_deaths_data_links.rdata") 
jhu_cases_data_links <- get_time_series_data_link(measure = "cases",
                                                  download_files = TRUE,
                                                  download_recent = download_recent)
save(jhu_cases_data_links, file = "data/jhu_cases_data_links.rdata")
