preview_column <- function(df, column) {
  if (!column %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }

  suppressWarnings(as.numeric(df[[column]]))
}

format_preview_elapsed <- function(seconds) {
  vapply(seconds, function(value) {
    if (!is.finite(value)) return(NA_character_)

    hours <- floor(value / 3600)
    minutes <- floor((value %% 3600) / 60)
    seconds <- floor(value %% 60)
    sprintf("%02d:%02d:%02d", hours, minutes, seconds)
  }, character(1))
}

activity_preview_data <- function(df, timezone = "Europe/Warsaw") {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(data.frame())
  }

  timestamps <- if (inherits(df$timestamp, "POSIXt")) {
    format(df$timestamp, "%Y-%m-%d %H:%M:%S", tz = timezone)
  } else {
    as.character(df$timestamp)
  }

  moving <- ifelse(
    is.na(df$moving),
    NA_character_,
    ifelse(df$moving, "tak", "nie")
  )

  preview <- data.frame(
    `Data i czas` = timestamps,
    `Od startu` = format_preview_elapsed(preview_column(df, "elapsed_seconds")),
    `Dystans [km]` = preview_column(df, "distance_km"),
    `Prędkość [km/h]` = preview_column(df, "speed_kmh"),
    `Tętno [bpm]` = preview_column(df, "heart_rate"),
    `Kadencja [rpm]` = preview_column(df, "cadence"),
    `Moc [W]` = preview_column(df, "power"),
    `Wysokość [m]` = preview_column(df, "altitude"),
    `Nachylenie [%]` = preview_column(df, "grade"),
    `Temperatura [°C]` = preview_column(df, "temperature"),
    `W ruchu` = moving,
    check.names = FALSE
  )

  primary_columns <- c(
    "Data i czas", "Od startu", "Dystans [km]", "Prędkość [km/h]"
  )
  has_values <- vapply(preview, function(column) {
    if (is.numeric(column)) any(is.finite(column)) else any(!is.na(column) & column != "")
  }, logical(1))

  optional_columns <- setdiff(names(preview)[has_values], c(primary_columns, "W ruchu"))
  preview[c(primary_columns, optional_columns, "W ruchu")]
}
