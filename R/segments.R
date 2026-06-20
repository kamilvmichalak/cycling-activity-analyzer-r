segment_value_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

segment_value_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else max(x)
}

segment_range <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else max(x) - min(x)
}

create_activity_segments <- function(
  df,
  by = c("distance", "time"),
  segment_length = 1
) {
  by <- match.arg(by)

  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop("Brak danych do utworzenia segmentów.", call. = FALSE)
  }

  if (!is.numeric(segment_length) || length(segment_length) != 1L ||
    !is.finite(segment_length) || segment_length <= 0) {
    stop("Długość segmentu musi być dodatnią liczbą.", call. = FALSE)
  }

  measure <- if (by == "distance") df$distance_km else df$elapsed_minutes
  valid <- is.finite(measure)
  if (!any(valid)) {
    stop(
      if (by == "distance") "Brak danych dystansu." else "Brak danych czasu.",
      call. = FALSE
    )
  }

  data <- df[valid, , drop = FALSE]
  measure <- measure[valid]
  segment_number <- floor((measure - min(measure)) / segment_length) + 1L
  groups <- split(seq_len(nrow(data)), segment_number)

  rows <- lapply(names(groups), function(group_name) {
    indexes <- groups[[group_name]]
    segment <- data[indexes, , drop = FALSE]
    segment_measure <- measure[indexes]
    timestamps <- segment$timestamp[!is.na(segment$timestamp)]

    start_time <- if (length(timestamps) > 0L) min(timestamps) else NA
    end_time <- if (length(timestamps) > 0L) max(timestamps) else NA
    duration <- if (length(timestamps) > 0L) {
      as.numeric(difftime(end_time, start_time, units = "secs"))
    } else {
      NA_real_
    }

    data.frame(
      segment = as.integer(group_name),
      start = min(segment_measure),
      end = max(segment_measure),
      start_time = start_time,
      end_time = end_time,
      distance_km = segment_range(segment$distance_km),
      duration_seconds = duration,
      average_speed_kmh = segment_value_mean(
        segment$speed_kmh[segment$moving %in% TRUE]
      ),
      max_speed_kmh = segment_value_max(segment$speed_kmh),
      average_heart_rate = segment_value_mean(segment$heart_rate),
      max_heart_rate = segment_value_max(segment$heart_rate),
      ascent_m = if (any(is.finite(segment$altitude))) {
        sum(segment$altitude_gain, na.rm = TRUE)
      } else {
        NA_real_
      }
    )
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  attr(result, "segment_by") <- by
  attr(result, "segment_length") <- segment_length
  result
}

segments_as_data_frame <- function(segments) {
  by <- attr(segments, "segment_by")
  unit <- if (identical(by, "distance")) "km" else "min"

  data.frame(
    Segment = segments$segment,
    Początek = paste0(round(segments$start, 2), " ", unit),
    Koniec = paste0(round(segments$end, 2), " ", unit),
    `Dystans [km]` = round(segments$distance_km, 2),
    Czas = vapply(segments$duration_seconds, format_duration, character(1)),
    `Średnia prędkość [km/h]` = round(segments$average_speed_kmh, 1),
    `Maksymalna prędkość [km/h]` = round(segments$max_speed_kmh, 1),
    `Średnie tętno [bpm]` = round(segments$average_heart_rate),
    `Maksymalne tętno [bpm]` = round(segments$max_heart_rate),
    `Podjazd [m]` = round(segments$ascent_m),
    check.names = FALSE
  )
}

plot_segments_speed <- function(segments) {
  valid <- is.finite(segments$average_speed_kmh)
  if (!any(valid)) {
    return(empty_plot("Brak danych prędkości dla segmentów"))
  }

  ggplot2::ggplot(
    segments[valid, , drop = FALSE],
    ggplot2::aes(x = factor(segment), y = average_speed_kmh)
  ) +
    ggplot2::geom_col(fill = "#2563EB") +
    ggplot2::labs(
      title = "Średnia prędkość w segmentach",
      x = "Numer segmentu",
      y = "Średnia prędkość [km/h]"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
