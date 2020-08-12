library(covidData)
library(dplyr)
library(here)

setwd(here())

# find  observations with negative inc
get_negative_cases <- function(data) {
  locations <- c()
  dates <- c()
  
  for (i in 1:nrow(data)) {
    if (data[i, ]$inc < 0) {
      locations <- c(locations, as.character(data[i, ]$location))
      dates <- c(dates, as.character(data[i, ]$date))
    }
  }
  
  adjustments <- data.frame(fips = locations, dates = as.Date(dates))
  
  return(adjustments)
}


# call stan model and get imputed value for a adjustment case
get_imputed_value <- function(data, adjustment_case, model) {

  inds <- as.Date(adjustment_case$date) - min(data$date)
  
  #int 0
  forecast_horizon <- 0L
  nsim <- 1000L
  knot_frequency <- 7L
  
  all_knots <- seq(
    from = nrow(data) %% knot_frequency,
    to = nrow(data) + forecast_horizon,
    by = knot_frequency)
  boundary_knots <- all_knots[c(1, length(all_knots))]
  interior_knots <- all_knots[-c(1, length(all_knots))]
  
  #par estimates....predictions
  map_estimates_daily <- rstan::optimizing(object = model, data = list(
    T = nrow(data),
    y = as.integer(data$inc),
    spline_order = 4L,
    n_interior_knots = length(interior_knots),
    interior_knots = interior_knots,
    boundary_knots = boundary_knots,
    forecast_horizon = forecast_horizon,
    nsim = nsim
  ),
  verbose = TRUE)
  
  
  results <- data.frame(
    t = seq_len(nrow(data)),
    y = data$inc,
    y_hat_daily = unname(map_estimates_daily$par[
      grepl("y_mean_with_daily", names(map_estimates_daily$par))])[seq_len(nrow(data))]
  )
  
  return(results[inds, ]$y_hat_daily)
}

# generate final output
get_results <- function(data, measure, model) {

  # find all cases
  na_adjustments <- get_negative_cases(data)
  
  results <-  data.frame(location = character(),
                         date = as.Date(character()), 
                         measure = character(), 
                         inc = integer()) 

  
  # replace negative values with 0 
  for (i in 1:nrow(na_adjustments)) {
    adjustment_location <- as.character(na_adjustments[i, ]$fips)
    adjustment_date <- na_adjustments[i, ]$date
    
    data <- data %>% 
      dplyr::mutate(inc = replace(inc, location == adjustment_location & 
                                    date == adjustment_date, 0))
  }
  
  if (measure == "deaths") {
    #only keeps state and national level
    na_adjustments <- dplyr::filter(na_adjustments, nchar(fips) == 2)
    adjustments <- rbind(adjustments, na_adjustments)
    
    # case that doesn't need adjustment 
    target <- data.frame(location = "48",
                        date = as.Date("2020-07-27"), 
                        measure = measure, 
                        inc = 44) 
    results <- rbind(results, target)
  } else{
    adjustments <- na_adjustments
    # case that doesn't need adjustment 
    target <- data.frame(location = "09",
                        date = as.Date("2020-07-29"), 
                        measure = measure, 
                        inc = 79) 
    results <- rbind(results, target)
  }
  
  
  # adjustments includes -inc cases
  for (i in 1:floor(nrow(adjustments) / 4)) {
    cat(i, file = "code/data-processing/log.txt", append = TRUE)
    adjustment_location <- adjustments[i, ]$fips
    adjustment_date <- as.Date(adjustments[i, ]$date)
    cat(paste(" adjustment_location", adjustment_location, sep = ": "), file = "code/data-processing/log.txt", append = TRUE)
    cat(paste(" adjustment_date", adjustment_date, sep = ": "), file = "code/data-processing/log.txt", append = TRUE)
    
    # get state, counties and national observations for an adjustment case
    location_data <- data %>%
      dplyr::filter(stringr::str_sub(location, start = 1, end = 2) %in% adjustment_location |
                           location == "US" | location == adjustment_location) %>%
      dplyr::group_by(location) %>%
      # only obs before adjustment_date would change
      dplyr::filter(date <= adjustment_date) %>%
      dplyr::ungroup()
  
    # for each location in data, get imputed data
    for (fips in unique(location_data$location)) {
      
      cat(paste(" imputing fips", fips, sep = ": "), file = "code/data-processing/log.txt", append = TRUE)
      cat(paste(" imputing date", adjustment_date, sep = ": "), file = "code/data-processing/log.txt", append = TRUE)
      
      d <- location_data[location_data$location == fips, ]
      
      set.seed(1234)
      # call stan model here
      imputed <- round(get_imputed_value(d, adjustments[i, ], model), digits = 0)
      
      # cols: location, dates, measure, inc
      target <- data.frame(location = fips,
                 date = adjustment_date, 
                 measure = measure, 
                 inc = imputed) 
      
      # add to final outputs
      results <- rbind(results, target)
    }
    
  }

  # change name and save
  if (measure == "deaths") {
    jhu_deaths_imputed_data <- results
    # need to change cols: location, dates, measure, inc
    save(jhu_deaths_imputed_data, file = "data/jhu_deaths_imputed_data.rdata")
  } else if (measure == "cases") {
    jhu_cases_imputed_data <- results
    # need to change cols: location, dates, measure, inc
    save(jhu_cases_imputed_data, file = "data/jhu_cases_imputed_data.rdata")
  }
}



# process all adjustment cases

# update this 
# TX-2020-07-27 death: 44
# CT-2020-07-29 case: 463-384
adjustment_cases <- c("CO-2020-04-24", "MS-2020-06-22",
                     "DE-2020-06-23", "NJ-2020-06-25")

adjustment_states <- sub("-.*", "", adjustment_cases)
adjustment_dates <- sub("^.*?-", "", adjustment_cases)
adjustment_state_fips <- unlist(lapply(
  adjustment_states, function(x) 
    covidData::fips_codes[which(covidData::fips_codes$abbreviation == x), ]$location))
adjustments <- data.frame(fips = adjustment_state_fips, dates = as.Date(adjustment_dates))

# read in data for all locations
death_data <- covidData::load_jhu_data(spatial_resolution = c("state", "county", "national"),
                        temporal_resolution = "daily",
                        measure = "deaths",
                        replace_negatives = FALSE,
                        adjustment_cases = "none")

case_data <- covidData::load_jhu_data(spatial_resolution = c("state", "county", "national"),
                          temporal_resolution = "daily",
                          measure = "cases",
                          replace_negatives = FALSE,
                          adjustment_cases = "none")

#compile stan model
model <- rstan::stan_model(file = "code/data-processing/bspline_forecast_daily.stan")

get_results(data = death_data, measure = "deaths", model = model)
get_results(data = case_data, measure = "cases", model = model)
