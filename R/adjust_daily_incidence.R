#' Redistribute difference between the original observation and the
#' imputed incidence value for the single day with reporting anomaly
#'
#' @param data time series data for one location related to an adjustment 
#' case. It has location, date, cum and inc as columns.
#' @param adjustment_date date in an adjustment case
#' @param measure case or death
#' @return incidence columns after redistribution
#' @export
adjust_daily_incidence <- function(data, adjustment_date, seed, measure) {
  set.seed(seed)

  obs <- data[which(data$date == as.Date(adjustment_date)), ]$inc

  # Read imputed data
  replacement <- round(covidData::impute_daily_incidence(data, adjustment_date, measure),
    digits = 0
  )
  
  if (obs > 0) {
    replacement <- ifelse(replacement > obs, obs, replacement)
  }
  
  # Get residual to redistribute
  diff <- obs - replacement

  # Replace observation with replacement value
  data[data$date == as.Date(adjustment_date),]$inc = replacement

  # Redistribute based on proportion
  new_cum <- sum(data[which(data$date <= as.Date(adjustment_date)), ]$inc)
 
  data <- data %>%
    dplyr::mutate(proportion = ifelse(date <= as.Date(adjustment_date),
      inc / new_cum, 0
    )) %>%
    dplyr::mutate(proportion = replace(proportion, is.nan(proportion), 0)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(inc = ifelse(date <= as.Date(adjustment_date),
      max(0, round(inc + diff * proportion, digits = 0)), inc
    )) %>%
    dplyr::ungroup() 
  
  # Get new cum count at adjustment_date
  new_cum <- sum(data[which(data$date <= as.Date(adjustment_date)), ]$inc)

  # Update residual to redistribute
  diff <- data[which(data$date == as.Date(adjustment_date)), ]$cum - new_cum

  # Get vector of indices sort data by proportion
  sorted_inds <- order(data[which(data$date <= as.Date(adjustment_date)), ]$proportion,
    decreasing = TRUE
  )
  
  # Redistribute residual to observations with the highest proportion
  inds_i <- 1
  while (diff != 0) {
    if (diff > 0) {
        data[sorted_inds[inds_i], ]$inc <- data[sorted_inds[inds_i], ]$inc + 1
        diff <- diff - 1
    } else if (diff < 0 & data[sorted_inds[inds_i], ]$inc > 0){
        # diff <0 & inc >0
        data[sorted_inds[inds_i], ]$inc <- data[sorted_inds[inds_i], ]$inc - 1
        diff <- diff + 1
    }
      
    # update index
    if (inds_i > length(sorted_inds) ){
      inds_i <- 1
    } else{
      inds_i <- inds_i + 1
    }
  }

  return(data$inc)
}
