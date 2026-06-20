default_hr_zones <- function(max_hr) {
  if (!is.numeric(max_hr) || length(max_hr) != 1L ||
    !is.finite(max_hr) || max_hr <= 0) {
    stop("Tętno maksymalne musi być dodatnią liczbą.", call. = FALSE)
  }

  data.frame(
    zone = paste0("Z", 1:5),
    name = c("Regeneracja", "Tlenowa", "Tempo", "Próg", "Maksymalna"),
    lower_bpm = round(max_hr * c(0.50, 0.60, 0.70, 0.80, 0.90)),
    upper_bpm = round(max_hr * c(0.60, 0.70, 0.80, 0.90, 1.00)),
    color = c("#60A5FA", "#22C55E", "#EAB308", "#F97316", "#DC2626"),
    stringsAsFactors = FALSE
  )
}

heart_zone_mean <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

calculate_heart_rate_zones <- function(df, max_hr = 190) {
  zones <- default_hr_zones(max_hr)

  if (!is.data.frame(df) || !"heart_rate" %in% names(df) ||
    !any(is.finite(df$heart_rate))) {
    return(data.frame())
  }

  heart_rate <- suppressWarnings(as.numeric(df$heart_rate))
  intervals <- if (inherits(df$timestamp, "POSIXt") && nrow(df) > 1L) {
    c(diff(as.numeric(df$timestamp)), 0)
  } else {
    rep(1, nrow(df))
  }
  intervals[!is.finite(intervals) | intervals < 0 | intervals > 10] <- 0

  distance_delta <- c(diff(df$distance_km), 0)
  distance_delta[!is.finite(distance_delta) | distance_delta < 0] <- 0

  breaks <- max_hr * c(-Inf, 0.60, 0.70, 0.80, 0.90, Inf)
  assigned_zone <- cut(
    heart_rate,
    breaks = breaks,
    labels = zones$zone,
    right = FALSE,
    ordered_result = TRUE
  )

  rows <- lapply(seq_len(nrow(zones)), function(index) {
    in_zone <- !is.na(assigned_zone) & assigned_zone == zones$zone[index]

    data.frame(
      zone = zones$zone[index],
      name = zones$name[index],
      lower_bpm = zones$lower_bpm[index],
      upper_bpm = zones$upper_bpm[index],
      time_seconds = sum(intervals[in_zone], na.rm = TRUE),
      average_heart_rate = heart_zone_mean(heart_rate[in_zone]),
      distance_km = sum(distance_delta[in_zone], na.rm = TRUE),
      average_speed_kmh = heart_zone_mean(df$speed_kmh[in_zone]),
      color = zones$color[index],
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  total_time <- sum(result$time_seconds)
  result$percentage <- if (total_time > 0) {
    result$time_seconds / total_time * 100
  } else {
    0
  }
  rownames(result) <- NULL
  result
}

heart_zones_as_data_frame <- function(zones) {
  if (!is.data.frame(zones) || nrow(zones) == 0L) {
    return(data.frame())
  }

  data.frame(
    Strefa = paste(zones$zone, zones$name),
    `Zakres [bpm]` = paste0(zones$lower_bpm, "–", zones$upper_bpm),
    Czas = vapply(zones$time_seconds, format_duration, character(1)),
    `Czas [%]` = round(zones$percentage, 1),
    `Średnie tętno [bpm]` = round(zones$average_heart_rate),
    `Dystans [km]` = round(zones$distance_km, 2),
    `Średnia prędkość [km/h]` = round(zones$average_speed_kmh, 1),
    check.names = FALSE
  )
}

plot_heart_rate_zones <- function(zones) {
  if (!is.data.frame(zones) || nrow(zones) == 0L) {
    return(empty_plot("Brak danych tętna w aktywności"))
  }

  zones$zone_label <- factor(
    paste(zones$zone, zones$name),
    levels = paste(zones$zone, zones$name)
  )

  ggplot2::ggplot(zones, ggplot2::aes(x = zone_label, y = time_seconds / 60)) +
    ggplot2::geom_col(fill = zones$color) +
    ggplot2::labs(
      title = "Czas w strefach tętna",
      x = "Strefa",
      y = "Czas [min]"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
}
