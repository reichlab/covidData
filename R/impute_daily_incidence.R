#' Imputed incidence value for the single day with reporting anomaly
#' 
#' @param data time series 
#' @param adjustment_case
#' @return 

#one location 
#precalculate output for all counties and states in this adjustment_case
impute_daily_incidence <- function (data, adjustment_case){
  
  # data <-load_jhu_data(
  #   #  issue_date = '2020-07-05',
  #   spatial_resolution = 'national',
  #   temporal_resolution = 'daily',
  #   measure = 'deaths')
  
  inds = as.Date(adjustment_case$date) - min(data$date)
  
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
  
  #compile
  model <- rstan::stan_model(file = "R/bspline_forecast_daily.stan")
  #par estimates....predictions
  map_estimates_daily <- rstan::optimizing(object = model, data= list(
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
  
  
  to_plot_daily_means <- data.frame(
    t = seq_len(nrow(data)),
    y = data$inc,
    y_hat_daily = unname(map_estimates_daily$par[
      grepl('y_mean_with_daily', names(map_estimates_daily$par))])[seq_len(nrow(data))]
  )
  
  return (to_plot_daily_means[inds,]$y_hat_daily)
}
