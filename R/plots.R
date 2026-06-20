empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 5) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
}

activity_axis <- function(df) {
  if ("distance_km" %in% names(df) && any(is.finite(df$distance_km))) {
    return(list(values = df$distance_km, label = "Dystans [km]"))
  }

  if ("elapsed_minutes" %in% names(df) && any(is.finite(df$elapsed_minutes))) {
    return(list(values = df$elapsed_minutes, label = "Czas [min]"))
  }

  list(values = seq_len(nrow(df)), label = "Numer rekordu")
}

plot_activity_series <- function(df, column, title, y_label, color) {
  if (!is.data.frame(df) || !column %in% names(df)) {
    return(empty_plot(paste("Brak danych:", y_label)))
  }

  axis <- activity_axis(df)
  values <- suppressWarnings(as.numeric(df[[column]]))
  plot_data <- data.frame(x = axis$values, value = values)
  plot_data <- plot_data[is.finite(plot_data$x) & is.finite(plot_data$value), ]

  if (nrow(plot_data) == 0L) {
    return(empty_plot(paste("Brak danych:", y_label)))
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value)) +
    ggplot2::geom_line(color = color, linewidth = 0.55, na.rm = TRUE) +
    ggplot2::labs(title = title, x = axis$label, y = y_label) +
    ggplot2::theme_minimal(base_size = 12)
}

plot_speed <- function(df) {
  plot_activity_series(
    df, "speed_kmh", "Prędkość podczas aktywności", "Prędkość [km/h]", "#2563EB"
  )
}

plot_heart_rate <- function(df) {
  plot_activity_series(
    df, "heart_rate", "Tętno podczas aktywności", "Tętno [bpm]", "#DC2626"
  )
}

plot_altitude <- function(df) {
  if (!is.data.frame(df) || !"altitude" %in% names(df) ||
    !any(is.finite(df$altitude))) {
    return(empty_plot("Brak danych wysokości"))
  }

  axis <- activity_axis(df)
  plot_data <- data.frame(x = axis$values, altitude = as.numeric(df$altitude))
  plot_data <- plot_data[is.finite(plot_data$x) & is.finite(plot_data$altitude), ]

  ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = altitude)) +
    ggplot2::geom_area(fill = "#86EFAC", alpha = 0.55) +
    ggplot2::geom_line(color = "#15803D", linewidth = 0.5) +
    ggplot2::labs(
      title = "Profil wysokości",
      x = axis$label,
      y = "Wysokość [m]"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

route_data <- function(df, max_points = 5000L) {
  if (!is.data.frame(df) ||
    !all(c("position_lat", "position_long") %in% names(df))) {
    return(data.frame())
  }

  route <- df[
    is.finite(df$position_lat) & is.finite(df$position_long),
    c("position_lat", "position_long"),
    drop = FALSE
  ]

  if (nrow(route) > max_points) {
    indexes <- unique(round(seq(1, nrow(route), length.out = max_points)))
    route <- route[indexes, , drop = FALSE]
  }

  route
}

plot_route <- function(df) {
  route <- route_data(df)
  if (nrow(route) < 2L) {
    return(empty_plot("Brak danych GPS"))
  }

  ggplot2::ggplot(
    route,
    ggplot2::aes(x = position_long, y = position_lat)
  ) +
    ggplot2::geom_path(color = "#1D4ED8", linewidth = 0.65) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(
      title = "Ślad GPS",
      x = "Długość geograficzna",
      y = "Szerokość geograficzna"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

create_route_map <- function(df) {
  route <- route_data(df)
  if (nrow(route) < 2L) {
    stop("Brak danych GPS do wyświetlenia mapy.", call. = FALSE)
  }

  map <- leaflet::leaflet(route)
  map <- leaflet::addTiles(map)
  map <- leaflet::addPolylines(
    map,
    lng = ~position_long,
    lat = ~position_lat,
    color = "#2563EB",
    weight = 4,
    opacity = 0.9
  )
  leaflet::fitBounds(
    map,
    lng1 = min(route$position_long),
    lat1 = min(route$position_lat),
    lng2 = max(route$position_long),
    lat2 = max(route$position_lat)
  )
}
