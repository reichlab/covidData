
# process all adjustment cases

#death+cases
# + cases for negative values state+ county 

#deaths
adjustment_cases <-c('CO-2020-04-24', 'MS-2020-06-22',
                     'DE-2020-06-23', 'NJ-2020-06-25')

adjustment_states = sub("-.*", "", adjustment_cases)
adjustment_dates = sub("^.*?-", "", adjustment_cases)
adjustment_state_fips = unlist(lapply(
  adjustment_states, function(x) 
    covidData::fips_codes[which(covidData::fips_codes$abbreviation==x),]$location))
#changed to as.Date
adjustments = data.frame(fips = adjustment_state_fips,dates = as.Date(adjustment_dates))

# read in data for all locations + replace <0 
death_data = load_jhu_data(spatial_resolution = c("state","county","national"),
                        temporal_resolution = 'daily',measure = 'deaths')

case_data = load_jhu_data(spatial_resolution = c("state","county","national"),
                          temporal_resolution = 'daily',measure = 'cases')

results <- NULL   

for (i in 1:nrow(adjustments)){
  adjustment_location = adjustments[i,]$fips
  adjustment_date = as.Date(adjustments[i,]$date)
  
  # get state, counties and national observations for an adjustment case
  data = dplyr::filter(death_data,stringr::str_sub(location, start = 1, end=2) %in% adjustment_location &
                         location == 'US')%>%
    dplyr::group_by(location)%>%
    # only obs before adjustment_date would change
    dplyr::filter(date <= adjustment_date)%>%
    dplyr::ungroup()

  # for each location in data, get imputed data
  for (fips in unique(data$location)){
    d = data[data$location == fips,]
    seed = 1234
    imputed = adjust_daily_incidence(d, adjustments[i,],seed) 
    results = rbind(results, imputed)
  }
  
}

# location, dates, measure, inc
save(results, file = 'data/jhu_deaths_imputed_data.rdata')


