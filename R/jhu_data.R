#' Construct table of truths for weekly incident and cumulative deaths at
#' horizons one though 6
#'
#' @param issue_date character issue date (i.e. report date) to use for
#' constructing truths in format 'yyyy-mm-dd'
#' @param spatial_resolution character vector specifying spatial unit types to
#' include: 'county', 'state' and/or 'national'
#' @param temporal_resolution character vector specifying temporal resolution
#' to include: currently only 'weekly' is supported
#' @param measure character vector specifying measure of covid prevalence:
#' 'deaths' or 'cases'
#' @param replace_negatives 
#' @param adjustment_cases character vector specifying times and locations with
#' reporting anomalies to adjust.  Either 'none' (the default) or one or more
#' of 'CO-2020-04-24', 'MS-2020-06-22', 'DE-2020-06-23', 'NJ-2020-06-25'. These
#' refer to locations and times affected by reporting anomalies documented at
#' https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data#user-content-retrospective-reporting-of-probable-cases-and-deaths
#' @param adjustment_method string specifying how anomalies are adjusted.
#' 'fill_na' will replace affected observations with NAs and correct daily 
#' cumulative counts for all dates on and after adjustment date.
#' Currently the only option is 'impute_inc', which leaves cumulative counts
#' unaffected and imputes the incidence value with the 7-day mean incidence
#'
#' @return data frame with columns location (fips code), date, inc, and cum
#'
#' @export
load_jhu_data <- function(
  issue_date = NULL,
  spatial_resolution = 'state',
  temporal_resolution = 'weekly',
  measure = 'deaths',
  replace_negatives= TRUE,
  adjustment_cases = 'none',
  adjustment_method = 'impute_inc'
) {
  # validate measure and pull in correct data set
  measure <- match.arg(measure, choices = c('cases', 'deaths'))
  if(measure == 'cases') {
    jhu_data <- covidData::jhu_cases_data
  } else if(measure == 'deaths') {
    jhu_data <- covidData::jhu_deaths_data
  }
  
  # validate issue_date
  if(is.null(issue_date)) {
    issue_date <- max(jhu_data$issue_date)
  } else {
    issue_date <- as.character(lubridate::ymd(issue_date))
  }
  if(!(issue_date %in% jhu_data$issue_date)) {
    stop(paste0('Invalid issue date; must be one of: ',
                paste0(jhu_data$issue_date, collapse = ', ')))
  }
  
  # validate spatial_resolution
  spatial_resolution <- match.arg(
    spatial_resolution,
    choices = c('county', 'state', 'national'),
    several.ok = TRUE)
  
  # validate temporal_resolution
  temporal_resolution <- match.arg(
    temporal_resolution,
    choices = c('daily', 'weekly'),
    several.ok = FALSE
  )
  
  # get report for specified issue date
  jhu_data <- jhu_data %>%
    dplyr::filter(issue_date == UQ(issue_date)) %>%
    dplyr::pull(data) %>%
    `[[`(1) %>%
    tidyr::pivot_longer(
      matches('^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$'),
      names_to = 'date',
      values_to = 'cum') %>%
    dplyr::mutate(
      date = as.character(lubridate::mdy(date))
    )
  
  # summarized results for county level
  results <- NULL
  if('county' %in% spatial_resolution) {
    county_results <- jhu_data %>%
      dplyr::filter(FIPS > 100) %>%
      dplyr::mutate(
        location = sprintf("%05d", FIPS)) %>%
      dplyr::filter(location < '80001') %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(inc = diff(c(0,cum))) %>%
      dplyr::select(location, date, cum, inc) %>%
      dplyr::ungroup()
    
    results <- dplyr::bind_rows(results, county_results)
  }
  
  # summarized results for state level
  if('state' %in% spatial_resolution) {
    states_to_keep <- c(
      'Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California',
      'Colorado', 'Connecticut', 'Delaware', 'District of Columbia',
      'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois',
      'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine',
      'Maryland', 'Massachusetts', 'Michigan', 'Minnesota',
      'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada',
      'New Hampshire', 'New Jersey', 'New Mexico', 'New York',
      'North Carolina', 'North Dakota', 'Northern Mariana Islands',
      'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico',
      'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee',
      'Texas', 'Utah', 'Vermont', 'Virgin Islands', 'Virginia',
      'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
    
    state_results <- jhu_data %>%
      dplyr::filter(Province_State %in% states_to_keep) %>%
      dplyr::mutate(location_name = Province_State) %>%
      dplyr::group_by(location_name, date) %>%
      dplyr::summarize(cum = sum(cum)) %>%
      dplyr::group_by(location_name) %>%
      dplyr::mutate(inc = diff(c(0,cum))) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(covidData::fips_codes, by = 'location_name') %>%
      dplyr::select(location, date, cum, inc)
    
    results <- dplyr::bind_rows(results, state_results)
  }
  
  # summarized results for national level
  if('national' %in% spatial_resolution) {
    # because we don't filter on states_to_keep as above, we are off by a total
    # of 3 deaths attributed to Diamond Princess.
    national_results <- jhu_data %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(cum = sum(cum)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        inc = diff(c(0,cum)),
        location = 'US'
      ) %>%
      dplyr::select(location, date, cum, inc)
    
    results <- dplyr::bind_rows(results, national_results)
  }
  
  # if for neg
  
  # +function 
  # replace -inc --> 0 and put it on the day before it
  #find inds for -inc + redistribution
  #if (replace_negatives == TRUE){
  #  results = replace_negatives(data = results)
  #}
   
  # TODO: in results data frame, replace daily inc with NA in specific rows, if requested
  # at this point, the results data frame will have daily incidence values and we want to
  # replace the numbers in some rows with NAs (no new rows, editing existing rows)
  
   if (adjustment_cases !='none' & length(adjustment_cases)>0){
     adjustment_states = sub("-.*", "", adjustment_cases)
     adjustment_dates = sub("^.*?-", "", adjustment_cases)
     adjustment_state_fips = unlist(lapply(
       adjustment_states, function(x) 
         covidData::fips_codes[which(covidData::fips_codes$abbreviation==x),]$location))
     #changed to as.Date
     adjustments = data.frame(fips = adjustment_state_fips,dates = as.Date(adjustment_dates))
     if (adjustment_method=='fill_na'){
       results <- covidData::fill_na(results,adjustments)
     } 
     
     # has to be non-neg
     if (adjustment_method =='impute_and_redistribute'){
       #if (replace_negatives == FALSE){
       #   results = replace_negatives(data = results)
       #}
       
       for (i in 1:nrow(adjustments)){
         
         adjustment_location = adjustments[i,]$fips
         adjustment_date = as.Date(adjustments[i,]$date)
         
         # get state, counties and national observations for an adjustment case
         location_data = results %>%
           dplyr::filter(stringr::str_sub(location, start = 1, end=2) %in% adjustment_location |
                           location == 'US' | location == adjustment_location)
         
         # for each location in data, get imputed data
         for (fips in unique(location_data$location)){
           d = location_data[location_data$location == fips,]
           
           seed=1234
           # get adjusted inc column
           imputed_inc = adjust_daily_incidence(d, adjustments[i,],seed) 
           
           # put imputed data back to results
           results[which(results$location == fips),]$inc = imputed_inc
       }
      }
     }
     
   }      
  
  
  
  
  # TODO: if temporal_resolution == 'weekly', aggregate daily incidence to weekly incidence here
  # input: data frame with daily values, output: data frame with approximately 1/7 the # of rows
  # with weekly inc values
  # group_by(location, week), and summarize(inc = sum(inc, na.rm = TRUE))
  # if weekly temporal resolution, filter to saturdays
  # TODO: adjust data first, go to weekly resolution later
  if(temporal_resolution == 'weekly') {
    results <- results %>%
      dplyr::mutate(sat_date = lubridate::ceiling_date
                    (lubridate::ymd(date), unit = "week") - 1) %>%
      dplyr::group_by(location) %>%
      # if the last week is not complete, drop all obs in that week
      dplyr::filter(if (max(date)<max(sat_date)) date <= max(sat_date)-7 else TRUE) %>%
      dplyr::ungroup() %>%
      # delete date column
      dplyr::select(-2)%>%
      dplyr::rename (date = sat_date) %>%
      dplyr::group_by(location,date) %>%
      dplyr::summarize(inc = sum(inc, na.rm = FALSE))%>%
      dplyr::ungroup() 
  }
  
  # TODO: aggregate inc to get cum
  # at this point you'll have a data frame with results for all locations
  # and either an incorrect cum column that we need to replace (if temporal_resolution == daily)
  # or no cum column  (since previous step got rid of it if temporal_resolution == weekly)
  # group_by(location) [but not week] and summarize(cum = cumsum(inc))
  # add cumulative col
   results <- results %>%
     dplyr:: mutate(
       date = lubridate::ymd(date),
       cum = results %>%
         dplyr::group_by(location)%>%
         dplyr::mutate(cum = cumsum(inc)) %>%
         dplyr::ungroup() %>%
         dplyr::pull(cum))

  return(results)
}
