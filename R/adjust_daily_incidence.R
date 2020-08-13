#' Redistribute difference between the original observation and the
#' imputed incidence value for the single day with reporting anomaly
#'
#' @param data time series data for location in adjustment case
#' @param adjustment_case a case to adjust
#' @param measure case or death
#' @return incidence columns after redistribution
#' @export


adjust_daily_incidence <- function(data, adjustment_case, seed, measure) {
  set.seed(seed)

  obs <- data[which(data$date == as.Date(adjustment_case$date)), ]$inc

  # Read imputed data
  replacement <- round(impute_daily_incidence(data, adjustment_case, measure),
    digits = 0
  )
  # If rep > obs  repl= obs for stan
  if (obs > 0) {
    replacement <- ifelse(replacement > obs, obs, replacement)
  }
  diff <- obs - replacement

  # Replace observation with replacement value
  data <- data %>%
    dplyr::mutate(inc = replace(inc, (date == as.Date(adjustment_case$date) &
      stringr::str_sub(location, start = 1, end = 2) %in%
        adjustment_case$fips), replacement))

  # Redistribute based on proportion
  data <- data %>%
    dplyr::mutate(new_cum = ifelse(date <= as.Date(adjustment_case$date),
      cumsum(inc), 0
    )) %>%
    dplyr::mutate(proportion = ifelse(date <= as.Date(adjustment_case$date),
      inc / new_cum[date == as.Date(adjustment_case$date)], 0
    )) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(inc = ifelse(date <= as.Date(adjustment_case$date),
      max(0, round(inc + diff * proportion, digits = 0)), inc
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(location, date, cum, inc)

  # Get new cum count at adjustment_date
  new_cum <- sum(data[which(data$date <= as.Date(adjustment_case$date)), ]$inc)

  diff <- data[which(data$date == as.Date(adjustment_case$date)), ]$cum - new_cum

  # Get vector of indices sort data by inc
  sorted_inds <- order(data[which(data$date <= as.Date(adjustment_case$date)), ]$inc,
    decreasing = TRUE
  )

  # Redistribute residual to observations with the most inc
  inds_i <- 1
  while (diff != 0) {
    if (data[sorted_inds[inds_i], ]$inc >= 1) {
      if (diff > 0) {
        data[sorted_inds[inds_i], ]$inc <- data[sorted_inds[inds_i], ]$inc + 1
        diff <- diff - 1
      } else {
        data[sorted_inds[inds_i], ]$inc <- data[sorted_inds[inds_i], ]$inc - 1
        diff <- diff + 1
      }
      inds_i <- inds_i + 1
    } else {
      inds_i <- 1
    }
  }

  return(data$inc)
}
