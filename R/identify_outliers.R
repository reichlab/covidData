#' Identify outliers and suitable replacement values
#'
#' @param data time series data frame for one location with at minimum columns
#' \code{date} and \code{inc}
#' 
#' @return data frame with columns \code{date} and \code{inc} with only rows for
#' detected outliers.
#' 
#' @export
identify_outliers <- function(
  data,
  methods = data.frame(
    method = c(rep("tsoutliers", 2), rep("rolling_median", 2), "loess"),
    transform = c("none", "BoxCox", "log", "log_diff", "log")
  ),
  voting_scheme = "majority") {

  outliers_by_method <- purrr::pmap_dfr(
    methods,
    function(method, transform) {
      if (method == "tsoutliers") {
        identify_outliers_tsoutliers(data, transform) %>%
          dplyr::mutate(
            method_transform = paste(method, transform, sep = "_")
          )
      } else if (method == "rolling_median") {
        identify_outliers_rolling_median(data, transform) %>%
          dplyr::mutate(
            method_transform = paste(method, transform, sep = "_")
          )
      } else if (method == "loess") {
        identify_outliers_loess(data, transform) %>%
          dplyr::mutate(
            method_transform = paste(method, transform, sep = "_")
          )
      }
    }
  )

  outlier_counts <- table(outliers_by_method$date)
  large_counts <- outlier_counts[outlier_counts > nrow(methods)/2]

  combined_outliers <- outliers_by_method %>%
    dplyr::filter(as.character(date) %in% names(large_counts)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(inc = median(inc)) %>%
    dplyr::mutate(method_transform = "combined")

  return(dplyr::bind_rows(outliers_by_method, combined_outliers))
}

identify_outliers_tsoutliers <- function(data, transform = c("none", "BoxCox", "log")) {
  # detect whether we have daily or weekly data
  ts_freq <- 7 / as.integer(data$date[2] - data$date[1])

  # create ts object for data
  inc <- ts(data$inc, frequency = ts_freq)

  # convert transform argument to lambda suitable for forecast::tsoutliers
  if (transform == "none") {
    lambda <- NULL
  } else if (transform == "BoxCox") {
    lambda <- "auto"
  } else if (transform == "log") {
    lambda <- 0
  } else {
    stop("Invalid transform")
  }

  if (transform %in% c("BoxCox", "log")) {
    offset <- -1 * min(data$inc) + 1
    inc <- inc + offset
  }

  outliers <- forecast::tsoutliers(inc, lambda = lambda) %>%
    as.data.frame() %>%
    dplyr::transmute(
      date = data$date[index],
      inc = replacements
    )

  if (transform == "log") {
    outliers$inc <- outliers$inc - offset
  }

  return(outliers)
}

identify_outliers_rolling_median <- function(data, transform = "log_diff", multiplier = 3) {
  if (grepl("log", transform)) {
    offset <- -1 * min(data$inc) + 1
    data$inc <- log(data$inc + offset)
  } else if (!grepl("none", transform)) {
    stop("Invalid transform")
  }

  if (grepl("diff", transform)) {
    orig_inc <- data$inc
    ts_freq <- 7 / as.integer(data$date[2] - data$date[1])
    data$inc <- c(rep(NA, ts_freq), diff(data$inc, ts_freq))
  }

  outliers <- data %>%
    dplyr::mutate(
      roll_median = rollapply(data = inc, width = 14, median, na.rm = TRUE, align = "right", fill = 0),
      roll_iqr = rollapply(data = inc, width = 14, IQR, na.rm = TRUE, align = "right", fill = 0),
      lower_limit = roll_median - multiplier * roll_iqr,
      upper_limit = roll_median + multiplier * roll_iqr,
      outlier = (inc < lower_limit) | (inc > upper_limit)
    )

  if (grepl("diff", transform)) {
    outliers$roll_median <- outliers$roll_median +
      c(rep(NA, ts_freq), orig_inc[seq_len(length(orig_inc) - ts_freq)])
  }

  outliers <- outliers %>%
    dplyr::filter(outlier) %>%
    dplyr::transmute(
      date = date,
      inc = roll_median
    )

  if (grepl("log", transform)) {
    outliers$inc <- exp(outliers$inc) - offset
  }

  return(outliers)
}


identify_outliers_loess <- function(data, transform = "log", sd_multiplier = 3) {
  if (transform == "log") {
    offset <- -1 * min(data$inc) + 1
    data$inc <- log(data$inc + offset)
  } else if (transform != "none") {
    stop("Invalid transform")
  }

  data$index <- seq_len(nrow(data))

  ts_freq <- 7 / as.integer(data$date[2] - data$date[1])
  loess_fit <- loess(inc ~ index, data = data, span = ts_freq * 4 / nrow(data))

  outliers <- data %>%
    dplyr::mutate(
      loess_pred = predict(loess_fit, index),
      lower_limit = loess_pred - sd_multiplier * loess_fit$s,
      upper_limit = loess_pred + sd_multiplier * loess_fit$s,
      outlier = (inc < lower_limit) | (inc > upper_limit)
    ) %>%
    dplyr::filter(outlier) %>%
    dplyr::transmute(
      date = date,
      inc = loess_pred
    )
  
  if (transform == "log") {
    outliers$inc <- exp(outliers$inc) - offset
  }

  return(outliers)
}
