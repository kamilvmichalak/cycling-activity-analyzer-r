source("R/import_fit.R", encoding = "UTF-8")
source("R/prepare_data.R", encoding = "UTF-8")
source("R/summary.R", encoding = "UTF-8")
source("R/plots.R", encoding = "UTF-8")
source("R/segments.R", encoding = "UTF-8")
source("R/heart_zones.R", encoding = "UTF-8")
source("R/data_preview.R", encoding = "UTF-8")
source("R/mock_data.R", encoding = "UTF-8")

raw_data <- create_mock_activity_data(n = 1000)
activity_data <- prepare_activity_data(raw_data)
activity_summary <- summarise_activity(activity_data)
distance_segments <- create_activity_segments(
  activity_data,
  by = "distance",
  segment_length = 1
)
time_segments <- create_activity_segments(
  activity_data,
  by = "time",
  segment_length = 5
)
heart_rate_zones <- calculate_heart_rate_zones(activity_data, max_hr = 190)
preview_data <- activity_preview_data(activity_data)
altitude_plot <- plot_altitude(activity_data)
altitude_axis_range <- ggplot2::ggplot_build(altitude_plot)$layout$panel_params[[1]]$y.range

stopifnot(
  is.data.frame(activity_data),
  nrow(activity_data) == 1000L,
  is.list(activity_summary),
  activity_summary$record_count == 1000L,
  nrow(distance_segments) > 0L,
  nrow(time_segments) > 0L,
  nrow(heart_rate_zones) == 5L,
  abs(sum(heart_rate_zones$percentage) - 100) < 0.01,
  nrow(preview_data) == 1000L,
  "Data i czas" %in% names(preview_data),
  !"left_right_balance" %in% names(preview_data),
  abs(max(altitude_axis_range) - max(activity_data$altitude, na.rm = TRUE)) < 0.001
)

message("Test dymny zakończony powodzeniem.")
