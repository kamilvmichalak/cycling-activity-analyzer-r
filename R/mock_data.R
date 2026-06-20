create_mock_activity_data <- function(n = 1000L, seed = 42L) {
  if (!is.numeric(n) || length(n) != 1L || n < 2L) {
    stop("Liczba rekordów danych testowych musi wynosić co najmniej 2.", call. = FALSE)
  }

  n <- as.integer(n)
  set.seed(seed)
  elapsed <- seq.int(0, n - 1L)
  phase <- seq(0, 4 * pi, length.out = n)

  speed <- pmax(0, 7.2 + 1.8 * sin(phase) + stats::rnorm(n, sd = 0.45))
  stop_starts <- if (n >= 180L) seq.int(180L, n, by = 300L) else integer()
  stop_indexes <- unlist(lapply(stop_starts, function(index) {
    seq(index, min(index + 9L, n))
  }), use.names = FALSE)
  speed[stop_indexes] <- 0

  distance <- cumsum(speed)
  heart_rate <- pmin(
    195,
    round(115 + speed * 7.5 + 8 * sin(phase / 2) + stats::rnorm(n, sd = 3))
  )
  cadence <- ifelse(speed > 1, round(78 + 8 * sin(phase) + stats::rnorm(n, sd = 3)), 0)
  power <- ifelse(speed > 1, pmax(0, round(80 + speed * 14 + stats::rnorm(n, sd = 18))), 0)

  data.frame(
    timestamp = as.POSIXct("2026-01-01 09:00:00", tz = "UTC") + elapsed,
    position_lat = 52.30 + 0.025 * sin(phase / 2),
    position_long = 16.95 + 0.045 * cos(phase / 2),
    distance = distance,
    altitude = 90 + 18 * sin(phase / 3) + 4 * sin(phase),
    speed = speed,
    heart_rate = heart_rate,
    cadence = cadence,
    power = power,
    temperature = round(22 + elapsed / max(elapsed) * 3),
    stringsAsFactors = FALSE
  )
}
