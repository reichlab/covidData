#' Imputed incidence value for the single day with reporting anomaly
#' 
#' @param data time series 
#' @param adjustment_case
#' @return 

#one location 
#precalculate output for all counties and states in this adjustment_case
impute_daily_incidence <- function (data, adjustment_case,measure){
  if (measure == 'death'){
    imputed_data <- get(load("data/jhu_deaths_imputed_data.rdata"))
  } else {
    # imputed case data
  }
  
  location = adjustment_case$fips
  date = adjustment_case$date
  
  target = imputed_data[which(imputed_data$location == location, 
                              imputed_data$date == date),]$inc
  
  return (target)
}
