library(httr)
library(dplyr)
library(here)
library(covidData)

setwd(here())
get_time_series_data_link <- function(measure, first_page_only = FALSE){
  
  if (measure == "deaths"){
    query_base_file <- "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
    if (file.exists("data/jhu_deaths_data_links.rdata")){
      links <- covidData::jhu_deaths_data_links
      head = max(links$date)
    } else {
      links <- data.frame()
      head = NULL
    }
  } else if (measure == "cases") {
    query_base_file <- "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
    if (file.exists("data/jhu_cases_data_links.rdata")){
      links <- covidData::jhu_cases_data_links
      head = max(links$date)
    } else {
      links <- data.frame()
      head = NULL
    }
  }
  
  page = 1
  stop = FALSE
  while (TRUE) {
    request <- httr::GET(
      'https://api.github.com/repos/CSSEGISandData/COVID-19/commits',
      query = list(path = query_base_file, 
                   sort = "author-date",
                   page = as.character(page)))
    if (request$status_code > 400 || stop) {
      print("breaking")
      break
    }
    content <- httr::content(request)
    
    if(length(content) == 0){
      break
    }
    
    for (i in 1:length(content)){
      curr_date = as.Date(sub("\\T.*", "", content[[i]]$commit$author$date))
      if (!(curr_date %in% links$date)){
        a_commit <- data.frame(date = content[[i]]$commit$author$date,
                               sha = content[[i]]$sha) %>%
                    tidyr::separate(date, into = c("date", "time"), sep = "T") %>%
                    dplyr::mutate(time =  sub("\\Z.*", "",time),
                                  date = as.Date(date),
                                  file_link = paste0(
                                    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/",
                                    sha,"/",query_base_file)) %>%
                   dplyr::select(-sha, -time)
          
        
        links <- links %>% dplyr::bind_rows(a_commit)
      } else {
        # the latest day in links
        if (curr_date == max(links$date)){
          sha <- content[[i]]$sha
          links[links$date == curr_date,]$file_link <- paste0(
            "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/",
            sha,"/",query_base_file)
          stop = TRUE
          break
        } 
        
        if (!is.null(head)){
          if (curr_date == head) {
            sha <- content[[i]]$sha
            links[links$date == curr_date,]$file_link <- paste0(
              "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/",
              sha,"/",query_base_file)
            
            print("Loaded all links. Breaking now")
            stop = TRUE
            break
          }
        }
      }
    }
    
    if(first_page_only){
      break
    } else {
      page <- page + 1
    }
  }
  return(links)
}


jhu_deaths_data_links <- get_time_series_data_link(measure = "deaths")
save(jhu_deaths_data_links, file = "data/jhu_deaths_data_links.rdata") 
jhu_cases_data_links <- get_time_series_data_link(measure = "cases")
save(jhu_cases_data_links, file = "data/jhu_cases_data_links.rdata")

# download files
