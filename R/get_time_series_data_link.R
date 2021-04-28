#' Assemble a data frame that stores all likes to JHU data series files and 
#' download truth data issued in the most recent week/day.
#' 
#' @param measure string specifying measure of covid dynamics: 
#' one of 'deaths' or 'cases'
#' @param hub character, which hub to use. Default is "US", other option is
#' "ECDC"
#' @param first_page_only boolean specify whether to only scrape 
#' the first page of github repo. Default to FALSE that scrapes all history
#' @param download_files boolean specify whether to download truth files after
#' scraping file links. Default to FALSE
#' @param download_recent boolean specify whether to download the most 
#' recent truth file only or all truth files in the most recent week. 
#' Default to TRUE to download the most recent file. 
#' 
#' @return a data frame with columns date and file_links
get_time_series_data_link <- function(measure, 
                                      hub = c("US", "ECDC"),
                                      first_page_only = FALSE,
                                      download_files = FALSE,
                                      download_recent = TRUE){
  if (measure == "deaths"){
    if (hub[1] == "US"){
      base_file <- "time_series_covid19_deaths_US.csv"
      links_file_path = file.path(paste0("data/", "jhu_us_deaths_data_links",".rdata"))
    } else if (hub[1] == "ECDC"){
      base_file <- "time_series_covid19_deaths_global.csv"
      links_file_path = file.path(paste0("data/", "jhu_ecdc_deaths_data_links",".rdata"))
    }
  } else if (measure == "cases") {
    if (hub[1] == "US"){
      base_file <- "time_series_covid19_confirmed_US.csv"
      links_file_path = file.path(paste0("data/", "jhu_us_cases_data_links",".rdata"))
    } else if (hub[1] == "ECDC"){
      base_file <- "time_series_covid19_confirmed_global.csv"
      links_file_path = file.path(paste0("data/", "jhu_ecdc_cases_data_links",".rdata"))
    }
  }
    
  if (file.exists(links_file_path)){
    links <- load(links_file_path)
    head <- max(links$date)
  } else {
    links <- data.frame()
    head <- NULL
  }

  query_base_file <- paste0("csse_covid_19_data/csse_covid_19_time_series/", base_file)
  page = 1
  stop = FALSE
  loaded_latest = FALSE
  while (TRUE) {
    request <- httr::GET(
      'https://api.github.com/repos/CSSEGISandData/COVID-19/commits',
      query = list(path = query_base_file, 
                   sort = "author-date",
                   page = as.character(page)))
    if (request$status_code > 400 || stop) {
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
        if (curr_date == max(links$date)){
          loaded_latest = TRUE
        }
      } else {
        # keep scraping until curr_date is the latest date in previous result
        if (!is.null(head)){
          if (curr_date == head) {
            sha <- content[[i]]$sha
            links[links$date == curr_date,]$file_link <- paste0(
              "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/",
              sha,"/",query_base_file)
            
            stop = TRUE
            break
          }
        }
        
        # replace link for the most recent day with the newly updated link
        if (curr_date == max(links$date) && (!loaded_latest)){
          sha <- content[[i]]$sha
          links[links$date == curr_date,]$file_link <- paste0(
            "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/",
            sha,"/",query_base_file)
          
          loaded_latest = TRUE
          next 
        } 
      }
    }
    
    if(first_page_only){
      break
    } else {
      page <- page + 1
    }
  }
  
  
  # sort data frame by dates
  links <- links[order(links$date, decreasing = TRUE), ]
  
  if (download_files){
    # select issue dates to download
    if (download_recent){
      download_dates <- links$date[1]
    } else{
      #download the data in the most recent week
      download_dates <- links$date[1:7]
    }
    
    dir <- file.path("data-raw", "JHU")
    
    # create data-raw/JHU folder if it does not exist
    if(!dir.exists(dir)){
      dir.create(dir)
    }
    
    for (date in as.character(download_dates)){
      date <- as.Date(date)
      
      destination_path <- file.path(
        dir, paste0(as.character(date), "_",base_file)
      )
      
      # if file has not been downloaded or the current date is the most recent date
      if (!file.exists(destination_path) || date == max(links$date)) {
        link <- links[links$date == date,]$file_link
        time_series_data <- suppressMessages(readr::read_csv(link))
        readr::write_csv(time_series_data, destination_path)
      }
    }
  }
  
  return(links)
}
