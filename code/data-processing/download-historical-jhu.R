library(httr)
library(dplyr)
library(here)

setwd(here())
source("R/get_time_series_data_link.R")

# parse argument for download_recent parameter in get_time_series_data_link
args <- (commandArgs(TRUE))
download_recent <- args[1]
# get US data links
jhu_us_deaths_data_links <- get_time_series_data_link(measure = "deaths", 
                                                      geography = "US",
                                                      download_files = TRUE,
                                                      download_recent = download_recent)
save(jhu_us_deaths_data_links, file = "data/jhu_us_deaths_data_links.rdata") 
jhu_us_cases_data_links <- get_time_series_data_link(measure = "cases",
                                                     geography = "US",
                                                     download_files = TRUE,
                                                     download_recent = download_recent)
save(jhu_us_cases_data_links, file = "data/jhu_us_cases_data_links.rdata")
# get global data links
jhu_global_deaths_data_links <- get_time_series_data_link(measure = "deaths", 
                                                          geography = "global",
                                                          download_files = TRUE,
                                                          download_recent = download_recent)
save(jhu_global_deaths_data_links, file = "data/jhu_global_deaths_data_links.rdata") 
jhu_global_cases_data_links <- get_time_series_data_link(measure = "cases",
                                                         geography =  "global",
                                                         download_files = TRUE,
                                                         download_recent = download_recent)
save(jhu_global_cases_data_links, file = "data/jhu_global_cases_data_links.rdata")
