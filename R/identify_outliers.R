#' Identify outliers and suitable replacement values
#'
#' @param data time series data frame for one location with at minimum columns
#' \code{location}, \code{date} and \code{inc}
#' 
#' @return data frame with columns \code{date} and \code{inc} with only rows for
#' detected outliers.
#' 
#' @export
identify_outliers <- function(
  data,
  methods = data.frame(
    method = c(rep("weekly_extrema_loess_loo", 3)),
    transform = c("none", "sqrt", "log"),
  ),
  voting_scheme = "majority",
  max_iter = 100,
  include_outliers_by_method = TRUE) {
  # validate data were provided for one location
  if (length(unique(data$location)) != 1) {
    stop("The identify_outliers function expects to receive data for a single location.")
  }

  # null tibble of results
  combined_outliers <- tibble::tibble(
    date = lubridate::ymd("2021-01-01"),
    inc = 0,
    method_transform = "combined",
    location = "A",
    iter = 1L
  )[-1, ]

  # drop leading zeros
  if (all(data$inc == 0)) {
    return(combined_outliers)
  }
  start_ind <- min(which(data$inc > 0))
  data <- data[seq(from = start_ind, to = nrow(data)), ]

  # empirically, the tsoutliers method with no transformation doesn't work
  # well if the counts are small
  if(all(data$inc < 10)) {
    methods <- methods %>%
      dplyr::filter(!method == "tsoutliers" & transform == "none")
  }

  # iteratively identify outliers
  for (i in seq_len(max_iter)) {
    outliers_by_method <- purrr::pmap_dfr(
      methods,
      function(method, transform) {
        if (method == "tsoutliers") {
          identify_outliers_tsoutliers(data, transform, seasonal = !grepl("nonseasonal", method)) %>%
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
        } else if (method == "filter") {
          identify_outliers_filter(data, transform) %>%
            dplyr::mutate(
              method_transform = paste(method, transform, sep = "_")
            )
        } else if (method == "weekly_extrema_loess") {
          identify_outliers_weekly_extrema_loess(data, transform, loo = FALSE) %>%
            dplyr::mutate(
              method_transform = paste(method, transform, sep = "_")
            )
        } else if (method == "weekly_extrema_loess_loo") {
          identify_outliers_weekly_extrema_loess(data, transform, loo = TRUE) %>%
            dplyr::mutate(
              method_transform = paste(method, transform, sep = "_")
            )
        }
      }
    )

    if (nrow(outliers_by_method) == 0) {
      break
    } else {
      # set location
      outliers_by_method <- outliers_by_method %>%
        dplyr::mutate(location = data$location[1])

      # add outliers detected by individual methods, if requested
      if (include_outliers_by_method) {
        combined_outliers <- dplyr::bind_rows(
            combined_outliers,
            outliers_by_method %>% dplyr::mutate(iter = i))
      }

      # add outliers that were identified by at least half of methods
      outlier_counts <- table(outliers_by_method$date)
      large_counts <- outlier_counts[outlier_counts >= nrow(methods) / 2]

      if (length(large_counts) == 0) {
        # if no new outliers were identified across all methods, stop looking
        break
      } else {
        # add outliers identified by all methods combined
        combined_outliers <- dplyr::bind_rows(
          combined_outliers,
          outliers_by_method %>%
            dplyr::filter(as.character(date) %in% names(large_counts)) %>%
            dplyr::group_by(date, location) %>%
            dplyr::summarize(inc = median(inc)) %>%
            dplyr::mutate(method_transform = "combined", iter = i)
        )

        # update data, replacing outliers with imputed values in preparation
        # for next iteration
        outlier_dates <- combined_outliers %>%
          dplyr::filter(method_transform == "combined") %>%
          dplyr::pull(date)

        data <- data %>%
          dplyr::filter(!(date %in% outlier_dates)) %>%
          dplyr::bind_rows(combined_outliers %>%
            dplyr::filter(method_transform == "combined") %>%
            dplyr::select(location, date, inc)) %>%
          dplyr::arrange(date)
      }
    }
  }

  return(combined_outliers)
}

identify_outliers_tsoutliers <- function(data, transform = c("none", "BoxCox", "log"), seasonal = TRUE) {
  # detect whether we have daily or weekly data
  if (seasonal) {
    ts_freq <- 7 / as.integer(data$date[2] - data$date[1])
  } else {
    ts_freq <- 1L
  }

  # create ts object for data
  inc <- ts(data$inc, frequency = ts_freq)

  # convert transform argument to lambda suitable for forecast::tsoutliers
  if (transform == "none") {
    lambda <- NULL
  } else if (transform == "BoxCox") {
    lambda <- "auto"
    lambda <- 0.25
  } else if (transform == "log") {
    lambda <- 0
  } else {
    stop("Invalid transform")
  }

  if (transform %in% c("BoxCox", "log")) {
#    data$inc[data$inc < 0]
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

  # method sometimes identifies "outliers" with the same imputed value as the
  # original data; drop those
  outliers <- outliers %>%
    dplyr::left_join(data, by = "date") %>%
    dplyr::filter(abs(inc.x - inc.y) > 1) %>%
    dplyr::transmute(
      date = date,
      inc = inc.x
    ) %>%
    dplyr::mutate(
      inc = pmax(inc, 0)
    )

  return(outliers)
}

loo_median <- function(x) {
  if (length(x) %% 2 != 1) {
    stop("x must have odd length")
  }
  midpoint_ind <- (length(x) - 1) / 2 + 1
  median(x[-midpoint_ind])
}

loo_quantile <- function(x, probs) {
  if (length(x) %% 2 != 1) {
    stop("x must have odd length")
  }
  midpoint_ind <- (length(x) - 1) / 2 + 1
  quantile(x[-midpoint_ind], probs = probs)
}

identify_outliers_rolling_median <- function(
  data,
  transform = "log_diff",
  multiplier = ifelse(grepl("log", transform), 2, 4)) {
  if (nrow(data) < 14) {
    return(
      data.frame(
        location = "A",
        date = lubridate::ymd("2021-01-01"),
        inc = 1
      )[-1, ]
    )
  }

  data$orig_inc_neg <- (data$inc < 0)
  if (grepl("log", transform)) {
    data$inc[data$inc < 0] <- 0L
    offset <- as.integer(any(data$inc == 0))
    data$inc <- log(data$inc + offset)
  } else if (!grepl("none", transform)) {
    stop("Invalid transform")
  }

  if (grepl("diff", transform)) {
    orig_inc <- data$inc
    ts_freq <- 7 / as.integer(data$date[2] - data$date[1])
    data$inc <- c(rep(NA, ts_freq), diff(data$inc, ts_freq))
  }

  data <- dplyr::bind_rows(
    data.frame(
      location = rep(data$location[1], 7),
      date = data$date[1] + seq(from = -7, to = -1),
      inc = rep(data$inc[1], 7),
      orig_inc_neg = rep(data$orig_inc_neg[1], 7)
    ),
    data,
    data[seq(from = nrow(data) - 14 + 1, to = nrow(data) - 7), ]
  )

  outliers <- data %>%
    dplyr::mutate(
      roll_median = rollapply(data = inc, width = 15, median, na.rm = TRUE, align = "center", fill = NA),
      roll_q25 = rollapply(data = inc, width = 15, quantile, probs = 0.25, na.rm = TRUE, align = "center", fill = NA),
      roll_q75 = rollapply(data = inc, width = 15, quantile, probs = 0.75, na.rm = TRUE, align = "center", fill = NA),
      roll_iqr = pmax(
        rollapply(data = inc, width = 15, IQR, na.rm = TRUE, align = "center", fill = NA),
        ifelse(grepl("log", transform), 0.5, 10)
      ),
      roll_sd = pmax(
        rollapply(data = inc, width = 15, sd, na.rm = TRUE, align = "center", fill = NA),
        ifelse(grepl("log", transform), 0.5, 10)
      ),
      lower_limit = roll_median - multiplier * roll_iqr,
      upper_limit = roll_median + multiplier * roll_iqr,
      low_outlier = (inc < lower_limit) | orig_inc_neg,
      high_outlier = (inc > upper_limit),
      outlier = low_outlier | high_outlier
    ) %>%
    # drop artificial rows at beginning and end
    dplyr::filter(!is.na(roll_median))

  # ggplot(data = outliers) + geom_line(mapping = aes(x = date, y = inc)) + geom_line(mapping = aes(x = date, y = roll_median), color = "orange") + geom_line(mapping = aes(x = date, y = lower_limit), color = "orange", linetype = 2) + geom_line(mapping = aes(x = date, y = upper_limit), color = "orange", linetype = 2)

  if (grepl("diff", transform)) {
    outliers$roll_median <- outliers$roll_median +
      c(rep(NA, ts_freq), orig_inc[seq_len(length(orig_inc) - ts_freq)])
  }

  outliers <- outliers %>%
    dplyr::filter(outlier) %>%
    dplyr::transmute(
      date = date,
      inc = ifelse(high_outlier, roll_median + roll_iqr, roll_median - roll_iqr)
    )

  if (grepl("log", transform)) {
    outliers$inc <- exp(outliers$inc) - offset
  }

  outliers$inc <- pmax(0, outliers$inc)

  return(outliers)
}


identify_outliers_loess <- function(data, transform = "log", multiplier = 4) {
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
      loess_resid = inc - loess_pred
    )
  resids_iqr <- IQR(outliers$loess_resid)
  resids_sd <- sd(outliers$loess_resid)
  outliers <- outliers %>%
    dplyr::mutate(
      lower_limit = loess_pred - multiplier * resids_iqr,
      upper_limit = loess_pred + multiplier * resids_iqr,
      outlier = (inc < lower_limit) | (inc > upper_limit)
    ) %>%
    dplyr::filter(outlier) %>%
    dplyr::transmute(
      date = date,
      inc = loess_pred
    )
#  ggplot(outliers) +geom_line(mapping =aes(x=date, y = inc)) + geom_line(mapping = aes(x = date, y = loess_pred), color = "orange") + geom_line(mapping = aes(x = date, y = lower_limit), color = "orange", linetype = 2) + geom_line(mapping = aes(x = date, y = upper_limit), color = "orange", linetype = 2)
  if (transform == "log") {
    outliers$inc <- exp(outliers$inc) - offset
  }

  return(outliers)
}


loess_predict <- function(x, incomplete_week, loo = FALSE) {
  # short-circuit if not enough data
  if (length(x) < 4) {
    return(x)
  }

  # augment data, prepending the second observation at the beginning and
  # the value from the second-to-last complete week at the end
  last_week_incomplete <- tail(incomplete_week, 1)
  data <- data.frame(
    index = c(0, seq_along(x), length(x) + 1),
    x = c(x[2], x, x[length(x) - 1 - last_week_incomplete]),
    x_hat = NA
  )
  if (nrow(data) < 25) {
    num_new_rows <- 25 - nrow(data)
    data <- dplyr::bind_rows(
      data.frame(
        index = rev(-1 * seq_len(num_new_rows)),
        x = rep(0, num_new_rows),
        x_hat = NA
      ),
      data
    )
  }

  if (loo) {
    # leave one out prediction -- to predict at index i,
    # fit loess leaving observation i out
    for (i in seq_along(x)) {
      loo_data <- data[data$index != i, ]
      if (last_week_incomplete && i == length(x) - 1) {
        loo_data <- loo_data[loo_data$index != i + 1, ]
      }
      loess_fit <- loess(x ~ index, data = loo_data, span = min(8 / nrow(data), 0.3))
      data$x_hat[data$index == i] <- predict(loess_fit, i)
    }
  } else {
    loess_fit <- loess(x ~ index, data = data, span = min(8 / nrow(data), 0.25))
    data$x_hat <- predict(loess_fit)
  }

  return(data$x_hat[data$index %in% seq_along(x)])
}


identify_outliers_weekly_extrema_loess <- function(
  data,
  transform = "log",
  multiplier = ifelse(grepl("log", transform), 2, 4),
  extrema_types = c("max", "min"),
  iter = 1,
  max_iter = 100,
  loo = FALSE) {
  if (length(extrema_types) > 1) {
    outliers <- purrr::map_dfr(
      extrema_types,
      identify_outliers_weekly_extrema_loess,
      data = data,
      transform = transform,
      multiplier = multiplier,
      iter = iter,
      max_iter = max_iter,
      loo = loo
    )
  } else {
    data$orig_inc <- data$inc
    data$orig_inc_neg <- (data$inc < 0)
    if (transform == "log") {
      data$inc[data$inc < 0] <- 0L
      offset <- as.integer(any(data$inc == 0))
      data$inc <- log(data$inc + offset)
    } else if (transform == "sqrt") {
      data$inc[data$inc < 0] <- 0L
      offset <- as.integer(any(data$inc == 0))
      data$inc <- sqrt(data$inc + offset)
    } else if (transform == "onefourth") {
      data$inc[data$inc < 0] <- 0L
      offset <- as.integer(any(data$inc == 0))
      data$inc <- (data$inc + offset)^0.25
    } else if (transform != "none") {
      stop("Invalid transform")
    }

    extreme_fun <- get(extrema_types)
    weekly_extreme_data <- data %>%
      dplyr::mutate(
        sat_date = lubridate::ceiling_date(
          lubridate::ymd(date), unit = "week") - 1
      ) %>%
  #    dplyr::group_by(location) %>%
      # if the last week is not complete, drop all observations from the
      # previous Saturday in that week
#      dplyr::filter(
#        if (max(date) < max(sat_date)) date <= max(sat_date) - 7 else TRUE
#      ) %>%
      dplyr::group_by(sat_date) %>%
      dplyr::mutate(incomplete_week = (n() < 7)) %>%
      dplyr::filter(inc == extreme_fun(inc)) %>%
      dplyr::ungroup() #%>%
      # dplyr::mutate(date = sat_date) %>%
      # dplyr::select(-sat_date)

    outliers <- weekly_extreme_data %>%
      dplyr::mutate(
        loess_pred = loess_predict(weekly_extreme_data$inc, weekly_extreme_data$incomplete_week, loo = loo),
        loess_resid = inc - loess_pred
      )
    
  #   ### START NEW
  #   outliers <- dplyr::bind_rows(
  #     data.frame(
  #       location = rep(data$location[1], 7),
  #       date = data$date[1] + seq(from = -7, to = -1),
  #       inc = rep(data$inc[1], 7),
  #       orig_inc_neg = rep(data$orig_inc_neg[1], 7)
  #     ),
  #     data,
  #     data[seq(from = nrow(data) - 14 + 1, to = nrow(data) - 7), ]
  #   )

  # outliers <- data %>%
  #   dplyr::mutate(
  #     roll_median = rollapply(data = inc, width = 15, median, na.rm = TRUE, align = "center", fill = NA),
  #     roll_q25 = rollapply(data = inc, width = 15, quantile, probs = 0.25, na.rm = TRUE, align = "center", fill = NA),
  #     roll_q75 = rollapply(data = inc, width = 15, quantile, probs = 0.75, na.rm = TRUE, align = "center", fill = NA),
  #     roll_iqr = pmax(
  #       rollapply(data = inc, width = 15, IQR, na.rm = TRUE, align = "center", fill = NA),
  #       ifelse(grepl("log", transform), 0.5, 5)
  #     ),
  #   ### END NEW

    resids_iqr <- max(
      IQR(outliers$loess_resid),
      ifelse(
        grepl("log", transform),
        0.1,
        ifelse(
          grepl("sqrt", transform),
          sqrt(3),
          ifelse(
            grepl("onefourth", transform),
            3^0.25,
            3
          )
        )
      )
    )
    #resids_sd <- sd(outliers$loess_resid)
    outliers <- outliers %>%
      dplyr::mutate(
        lower_limit = loess_pred - multiplier * resids_iqr,
        upper_limit = loess_pred + multiplier * resids_iqr,
        outlier = (inc < lower_limit & extrema_types == "min") |
          (inc > upper_limit & extrema_types == "max") |
          orig_inc_neg,
        resid = inc - loess_pred
      ) %>%
      dplyr::filter(outlier) %>%
      dplyr::slice_max(abs(resid)) %>%
      dplyr::transmute(
        location = location,
        date = date,
        inc = loess_pred
      )

    # transform back to original data scale
    if (transform == "log") {
      outliers$inc <- exp(outliers$inc) - offset
    } else if (transform == "sqrt") {
      outliers$inc <- (outliers$inc)^2 - offset
    } else if (transform == "onefourth") {
      outliers$inc <- (outliers$inc)^4 - offset
    }

    # enforce that imputed values are non-negative
    outliers$inc <- pmax(outliers$inc, 0)

    if (iter < max_iter && nrow(outliers) > 0) {
      # merge detected outliers into original data and recurse
      outlier_dates <- outliers %>%
        dplyr::pull(date)

      imputed_data <- data %>%
        dplyr::filter(!(date %in% outlier_dates)) %>%
        dplyr::mutate(inc = orig_inc) %>%
        dplyr::bind_rows(outliers %>%
          dplyr::select(date, inc)) %>%
        dplyr::arrange(date)

      new_outliers <- identify_outliers_weekly_extrema_loess(
        data = imputed_data,
        transform = transform,
        multiplier = multiplier,
        extrema_types = extrema_types,
        iter = iter + 1,
        max_iter = max_iter,
        loo = loo)

      # update detected outliers with newly detected outliers
      outliers <- outliers %>%
        dplyr::filter(!(date %in% new_outliers$date)) %>%
        dplyr::bind_rows(new_outliers)
    }

  #  ggplot(outliers) +geom_line(mapping =aes(x=date, y = inc)) + geom_line(mapping = aes(x = date, y = loess_pred), color = "orange") + geom_line(mapping = aes(x = date, y = lower_limit), color = "orange", linetype = 2) + geom_line(mapping = aes(x = date, y = upper_limit), color = "orange", linetype = 2)
  }

  return(outliers)
}


identify_outliers_filter <- function(data, transform = "log", multiplier = 4) {
  if (transform == "log") {
    offset <- -1 * min(data$inc) + 1
    data$inc <- log(data$inc + offset)
  } else if (transform != "none") {
    stop("Invalid transform")
  }

  data$index <- seq_len(nrow(data))
  data$filtered_inc <- dplR::pass.filt(data$inc, W = 7, type = "low", method = "Butterworth")

  outliers <- data %>%
    dplyr::mutate(
      filter_resid = inc - filtered_inc
    )
  resids_iqr <- IQR(outliers$filter_resid)
#  resids_sd <- sd(outliers$filter_resid)
  outliers <- outliers %>%
    dplyr::mutate(
      lower_limit = filtered_inc - multiplier * resids_iqr,
      upper_limit = filtered_inc + multiplier * resids_iqr,
      outlier = (inc < lower_limit) | (inc > upper_limit)
    ) %>%
    dplyr::filter(outlier) %>%
    dplyr::transmute(
      date = date,
      inc = filtered_inc
    )
#  ggplot(outliers) +geom_line(mapping =aes(x=date, y = inc)) + geom_line(mapping = aes(x = date, y = loess_pred), color = "orange") + geom_line(mapping = aes(x = date, y = lower_limit), color = "orange", linetype = 2) + geom_line(mapping = aes(x = date, y = upper_limit), color = "orange", linetype = 2)
  if (transform == "log") {
    outliers$inc <- exp(outliers$inc) - offset
  }

  return(outliers)
}
