library(tidyverse)
library(ggnewscale)
library(covidData)
library(zoo)
library(foreach)
library(doParallel)

# number of cores to use for local runs
registerDoParallel(cores = 22L)


all_dates <- covidData::jhu_deaths_data %>%
  dplyr::filter(issue_date %>% lubridate::ymd() %>%
  lubridate::wday() == 2) %>%
  dplyr::pull(issue_date)

all_locations <- covidData::fips_codes %>%
  dplyr::filter(nchar(location) == 2) %>%
  dplyr::pull(location)

all_outliers_detected <- purrr::map_dfr(
  all_dates,
  function(date) {
    message(date)

    data <- covidData::load_jhu_data(
      as_of = date,
      spatial_resolution = c("national", "state"),
      temporal_resolution = "daily")

    all_date_outliers_detected <- foreach(location = all_locations, .combine = dplyr::bind_rows) %dopar% {
      location_data <- data %>% dplyr::filter(location == UQ(location))
      location_name <- covidData::fips_codes %>%
        dplyr::filter(location == UQ(location)) %>%
        dplyr::pull(location_name)
      message(paste0(location, ": ", location_name))

      # short circuit if no data for this location
      if (nrow(location_data) == 0) {
        return(tibble(
          date = date,
          location = location,
          location_name = location_name,
          outliers_detected = list(NULL),
          location_data = list(NULL)
        )[-1, ])
      }

      outliers_detected <- identify_outliers(
        location_data,
        methods = data.frame(
#          method = c(rep("weekly_extrema_loess_loo", 3)),
#          transform = c("none", "sqrt", "log")
#          method = c(rep("rolling_median", 2)),
#          transform = c("none", "log")
          method = c(rep("weekly_extrema_loess_loo", 3), rep("rolling_median", 2)),
          transform = c("none", "sqrt", "log", "none", "log")
          # method = c(rep("weekly_extrema_loess_loo", 2), rep("rolling_median", 2), rep("tsoutliers", 1)),
          # transform = c("none", "log", "none", "log", "log")
        ), # %>% filter(method == "weekly_extrema_loess_loo", transform == "log"),
        max_iter = 10
      )

      return(tibble(
        date = date,
        location = location,
        location_name = location_name,
        outliers_detected = list(outliers_detected),
        location_data = list(location_data)
      ))
    }

    all_date_outliers_detected
  })

plot_start_date <- lubridate::ymd("2020-01-01")
plot_end_date <- lubridate::ymd(max(all_dates))

#pdf("outliers_daily_median_only.pdf", width = 14, height = 9)
pdf("outliers_daily_median_and_loess.pdf", width = 14, height = 9)
#pdf("outliers_daily_loess.pdf", width = 14, height = 9)
#pdf("outliers_daily_median_loess_tsoutliers.pdf", width = 14, height = 9)
for(location in all_locations) {
  for(date in all_dates) {
    location_date_outliers <- all_outliers_detected %>%
      dplyr::filter(as.character(date) == UQ(date), location == UQ(location))

    if(nrow(location_date_outliers) > 0) {
      location_name <- location_date_outliers$location_name[[1]]
      outliers_detected <- location_date_outliers$outliers_detected[[1]]
      location_data <- location_date_outliers$location_data[[1]]
      message(paste0(location_name, ", ", date))

      if (!is.null(outliers_detected) && nrow(outliers_detected) > 0) {
        outlier_dates <- outliers_detected %>%
          dplyr::filter(method_transform == "combined") %>%
          dplyr::pull(date)

        final_imputed_data <- location_data %>%
          dplyr::filter(!(date %in% outlier_dates)) %>%
          dplyr::bind_rows(outliers_detected %>%
            dplyr::filter(method_transform == "combined") %>%
            dplyr::select(date, inc))
      } else {
        final_imputed_data <- location_data
      }

      combined_data <- dplyr::bind_rows(
        location_data %>% mutate(data = "original"),
        final_imputed_data %>% mutate(data = "imputed")
      )

      p <- ggplot() +
        geom_line(data = combined_data %>% dplyr::filter(date >= plot_start_date),
          mapping = aes(x = date, y = inc, color = data, linetype = data)) +
        scale_x_date(limits = c(plot_start_date, plot_end_date)) +
        ggtitle(paste(location_name, date, sep = ", ")) +
        theme_bw()

      if (!is.null(outliers_detected)) {
        p <- p +
          new_scale_color() +
          geom_point(data = outliers_detected %>% dplyr::filter(date >= plot_start_date),
            mapping = aes(x = date, y = inc, color = method_transform, shape = method_transform, size = method_transform)) +
          scale_size_manual(values = c("tsoutliers_none" = 0.75, "tsoutliers_BoxCox" = 0.75, "tsoutliers_log" = 0.75,
            "rolling_median_log" = 0.75, "rolling_median_none" = 0.75, "filter_log" = 0.75,
            "rolling_median_log_diff" = 0.75,
            "weekly_extrema_loess_log" = 0.75, "weekly_extrema_loess_none" = 0.75, "weekly_extrema_loess_sqrt" = 0.75, "weekly_extrema_loess_onefourth" = 0.75,
            "weekly_extrema_loess_loo_log" = 0.75, "weekly_extrema_loess_loo_none" = 0.75, "weekly_extrema_loess_loo_sqrt" = 0.75, "weekly_extrema_loess_loo_onefourth" = 0.75,
            "loess_log" = 0.75, "combined" = 2))
      }

      print(p)
    }
  }
}
dev.off()
