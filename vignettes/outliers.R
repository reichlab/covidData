library(tidyverse)
library(ggnewscale)
library(covidData)

location_dates <- tidyr::expand_grid(
  location = covidData::fips_codes %>% filter(nchar(location) == 2) %>% pull(location),
  date = covidData::jhu_deaths_data %>%
    filter(issue_date %>% lubridate::ymd() %>% lubridate::wday() == 2) %>%
    pull(issue_date)
)

plot_start_date <- lubridate::ymd("2020-01-01")
plot_end_date <- lubridate::ymd(max(location_dates$date))

pdf("outliers.pdf", width = 14, height = 4)
for(i in seq_len(nrow(location_dates))) {
  date <- location_dates$date[i]
  location <- location_dates$location[i]
  data <- covidData::load_jhu_data(
    as_of = date,
    spatial_resolution = ifelse(location == "US", "national", "state"),
    temporal_resolution = "daily") %>%
    dplyr::filter(location == UQ(location))

  outliers_detected <- identify_outliers(data)
  outlier_dates <- outliers_detected %>%
    dplyr::filter(method_transform == "combined") %>%
    dplyr::pull(date)

  final_imputed_data <- data %>%
    dplyr::filter(!(date %in% outlier_dates)) %>%
    dplyr::bind_rows(outliers_detected %>%
      dplyr::filter(method_transform == "combined") %>%
      dplyr::select(date, inc))

  combined_data <- dplyr::bind_rows(
    data %>% mutate(data = "original"),
    final_imputed_data %>% mutate(data = "imputed")
  )

  p <- ggplot() +
    geom_line(data = combined_data %>% dplyr::filter(date >= plot_start_date),
      mapping = aes(x = date, y = inc, color = data, linetype = data)) +
    new_scale_color() +
    geom_point(data = outliers_detected %>% dplyr::filter(date >= plot_start_date),
      mapping = aes(x = date, y = inc, color = method_transform, shape = method_transform, size = method_transform)) +
    scale_size_manual(values = c("tsoutliers_none" = 0.75, "tsoutliers_BoxCox" = 0.75,
      "rolling_median_log" = 0.75, "rolling_median_log_diff" = 0.75, "loess_log" = 0.75, "combined" = 2)) +
    scale_x_date(limits = c(plot_start_date, plot_end_date)) +
    ggtitle(paste(location, date, sep = ", ")) +
    theme_bw()

  print(p)
}
dev.off()
