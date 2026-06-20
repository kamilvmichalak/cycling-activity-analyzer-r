as_activity_timestamp <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "UTC"))
  }

  if (is.numeric(x)) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
  }

  suppressWarnings(as.POSIXct(x, tz = "UTC"))
}

validate_coordinates <- function(lat, lon) {
  is.finite(lat) & is.finite(lon) &
    lat >= -90 & lat <= 90 & lon >= -180 & lon <= 180
}

calculate_speed_from_distance <- function(df) {
  time_difference <- c(NA_real_, diff(as.numeric(df$timestamp)))
  distance_difference <- c(NA_real_, diff(df$distance))
  calculated_speed <- distance_difference / time_difference

  invalid <- !is.finite(calculated_speed) |
    time_difference <= 0 |
    distance_difference < 0
  calculated_speed[invalid] <- NA_real_

  calculated_speed
}

calculate_altitude_gain <- function(altitude) {
  difference <- c(NA_real_, diff(altitude))
  ifelse(is.finite(difference) & difference > 0, difference, 0)
}

prepare_activity_data <- function(df) {
  if (!is.data.frame(df)) {
    stop("Dane aktywności muszą mieć postać data.frame.", call. = FALSE)
  }

  if (nrow(df) == 0L) {
    stop("Dane aktywności są puste.", call. = FALSE)
  }

  required <- c(
    "timestamp", "position_lat", "position_long", "distance",
    "altitude", "speed", "heart_rate", "cadence", "power", "temperature"
  )
  for (column in setdiff(required, names(df))) {
    df[[column]] <- NA
  }

  df$timestamp <- as_activity_timestamp(df$timestamp)
  df <- df[order(df$timestamp, na.last = TRUE), , drop = FALSE]
  rownames(df) <- NULL

  valid_time <- !is.na(df$timestamp)
  if (any(valid_time)) {
    start_time <- min(df$timestamp[valid_time])
    df$elapsed_seconds <- as.numeric(difftime(df$timestamp, start_time, units = "secs"))
  } else {
    df$elapsed_seconds <- rep(NA_real_, nrow(df))
  }

  df$elapsed_minutes <- df$elapsed_seconds / 60
  df$elapsed_hours <- df$elapsed_seconds / 3600

  df$distance <- suppressWarnings(as.numeric(df$distance))
  df$distance[df$distance < 0] <- NA_real_
  df$distance_km <- df$distance / 1000

  df$speed <- suppressWarnings(as.numeric(df$speed))
  calculated_speed <- calculate_speed_from_distance(df)
  missing_speed <- !is.finite(df$speed)
  df$speed[missing_speed] <- calculated_speed[missing_speed]
  df$speed_kmh <- df$speed * 3.6
  df$speed_kmh[df$speed_kmh < 0 | df$speed_kmh > 150] <- NA_real_

  distance_difference <- c(NA_real_, diff(df$distance))
  moving_from_distance <- is.finite(distance_difference) & distance_difference > 0
  df$moving <- ifelse(
    is.finite(df$speed_kmh),
    df$speed_kmh > 1,
    moving_from_distance
  )

  df$altitude <- suppressWarnings(as.numeric(df$altitude))
  df$altitude_gain <- calculate_altitude_gain(df$altitude)

  df$position_lat <- suppressWarnings(as.numeric(df$position_lat))
  df$position_long <- suppressWarnings(as.numeric(df$position_long))
  df$gps_valid <- validate_coordinates(df$position_lat, df$position_long)
  df$position_lat[!df$gps_valid] <- NA_real_
  df$position_long[!df$gps_valid] <- NA_real_

  df
}
