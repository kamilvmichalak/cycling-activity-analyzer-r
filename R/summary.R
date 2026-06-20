finite_values <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

safe_mean <- function(x) {
  x <- finite_values(x)
  if (length(x) == 0L) NA_real_ else mean(x)
}

safe_max <- function(x) {
  x <- finite_values(x)
  if (length(x) == 0L) NA_real_ else max(x)
}

safe_min <- function(x) {
  x <- finite_values(x)
  if (length(x) == 0L) NA_real_ else min(x)
}

format_duration <- function(seconds) {
  if (length(seconds) == 0L || !is.finite(seconds)) {
    return(NA_character_)
  }

  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  seconds <- floor(seconds %% 60)
  sprintf("%02d:%02d:%02d", hours, minutes, seconds)
}

calculate_moving_time <- function(df, maximum_interval = 10) {
  if (!inherits(df$timestamp, "POSIXt") || nrow(df) < 2L) {
    return(NA_real_)
  }

  intervals <- c(diff(as.numeric(df$timestamp)), 0)
  valid <- is.finite(intervals) & intervals >= 0 & intervals <= maximum_interval
  moving <- !is.na(df$moving) & df$moving

  sum(intervals[valid & moving], na.rm = TRUE)
}

summarise_activity <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop("Nie można podsumować pustych danych aktywności.", call. = FALSE)
  }

  timestamps <- df$timestamp[!is.na(df$timestamp)]
  missing_time <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  start_time <- if (length(timestamps) > 0L) min(timestamps) else missing_time
  end_time <- if (length(timestamps) > 0L) max(timestamps) else missing_time
  total_time <- if (length(timestamps) > 0L) {
    as.numeric(difftime(end_time, start_time, units = "secs"))
  } else {
    NA_real_
  }

  moving_time <- calculate_moving_time(df)
  distance_min <- safe_min(df$distance_km)
  distance_max <- safe_max(df$distance_km)
  total_distance <- if (is.finite(distance_min) && is.finite(distance_max)) {
    distance_max - distance_min
  } else {
    NA_real_
  }

  average_speed <- if (is.finite(total_distance) &&
    is.finite(moving_time) && moving_time > 0) {
    total_distance / (moving_time / 3600)
  } else {
    safe_mean(df$speed_kmh[df$moving %in% TRUE])
  }

  moving_rows <- df$moving %in% TRUE
  cadence_rows <- moving_rows & is.finite(df$cadence) & df$cadence > 0

  list(
    start_time = start_time,
    end_time = end_time,
    total_time_seconds = total_time,
    moving_time_seconds = moving_time,
    distance_km = total_distance,
    average_speed_kmh = average_speed,
    max_speed_kmh = safe_max(df$speed_kmh),
    average_heart_rate = safe_mean(df$heart_rate),
    max_heart_rate = safe_max(df$heart_rate),
    average_cadence = safe_mean(df$cadence[cadence_rows]),
    average_power = safe_mean(df$power[moving_rows]),
    max_power = safe_max(df$power),
    total_ascent_m = if (any(is.finite(df$altitude))) {
      sum(df$altitude_gain, na.rm = TRUE)
    } else {
      NA_real_
    },
    record_count = nrow(df),
    has_gps = any(df$gps_valid %in% TRUE)
  )
}

summary_as_data_frame <- function(summary) {
  display_number <- function(value, digits = 1, suffix = "") {
    if (!is.finite(value)) return("brak danych")
    paste0(format(round(value, digits), nsmall = digits, decimal.mark = ","), suffix)
  }

  display_time <- function(value) {
    if (length(value) == 0L || is.na(value)) return("brak danych")
    format(value, "%Y-%m-%d %H:%M:%S %Z")
  }

  data.frame(
    Metryka = c(
      "Początek", "Koniec", "Czas całkowity", "Czas ruchu", "Dystans",
      "Średnia prędkość", "Maksymalna prędkość", "Średnie tętno",
      "Maksymalne tętno", "Średnia kadencja", "Średnia moc",
      "Maksymalna moc", "Suma podjazdów", "Liczba rekordów", "Dostępny GPS"
    ),
    Wartość = c(
      display_time(summary$start_time),
      display_time(summary$end_time),
      format_duration(summary$total_time_seconds),
      format_duration(summary$moving_time_seconds),
      display_number(summary$distance_km, 2, " km"),
      display_number(summary$average_speed_kmh, 1, " km/h"),
      display_number(summary$max_speed_kmh, 1, " km/h"),
      display_number(summary$average_heart_rate, 0, " bpm"),
      display_number(summary$max_heart_rate, 0, " bpm"),
      display_number(summary$average_cadence, 0, " rpm"),
      display_number(summary$average_power, 0, " W"),
      display_number(summary$max_power, 0, " W"),
      display_number(summary$total_ascent_m, 0, " m"),
      as.character(summary$record_count),
      if (isTRUE(summary$has_gps)) "tak" else "nie"
    ),
    check.names = FALSE
  )
}
