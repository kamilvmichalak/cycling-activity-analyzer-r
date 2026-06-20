activity_columns <- c(
  "timestamp",
  "position_lat",
  "position_long",
  "distance",
  "altitude",
  "speed",
  "heart_rate",
  "cadence",
  "power",
  "temperature"
)

semicircles_to_degrees <- function(x) {
  x * 180 / 2^31
}

combine_fit_records <- function(record_sets) {
  if (is.data.frame(record_sets)) {
    return(as.data.frame(record_sets))
  }

  if (!is.list(record_sets)) {
    stop("Pakiet FITfileR zwrócił nieobsługiwany format rekordów.", call. = FALSE)
  }

  record_sets <- Filter(is.data.frame, record_sets)
  if (length(record_sets) == 0L) {
    stop("Plik FIT nie zawiera rekordów aktywności.", call. = FALSE)
  }

  as.data.frame(dplyr::bind_rows(record_sets))
}

copy_activity_column <- function(df, target, candidates) {
  sources <- intersect(candidates, names(df))

  if (!target %in% names(df)) {
    if (length(sources) == 0L) {
      df[[target]] <- NA
      return(df)
    }

    df[[target]] <- df[[sources[[1L]]]]
    sources <- sources[-1L]
  }

  for (source in setdiff(sources, target)) {
    missing_target <- is.na(df[[target]])
    df[[target]][missing_target] <- df[[source]][missing_target]
  }

  df
}

standardize_fit_columns <- function(df) {
  if (!is.data.frame(df)) {
    stop("Dane FIT muszą mieć postać data.frame.", call. = FALSE)
  }

  names(df) <- make.names(
    tolower(gsub("[^[:alnum:]_]+", "_", names(df))),
    unique = TRUE
  )

  aliases <- list(
    timestamp = c("timestamp", "date_time", "datetime", "time"),
    position_lat = c("position_lat", "latitude", "lat"),
    position_long = c("position_long", "longitude", "lon", "long"),
    distance = c("distance", "total_distance"),
    altitude = c("altitude", "enhanced_altitude"),
    speed = c("speed", "enhanced_speed"),
    heart_rate = c("heart_rate", "heartrate", "hr"),
    cadence = c("cadence", "cadence256"),
    power = c("power", "watts"),
    temperature = c("temperature", "temp")
  )

  for (target in names(aliases)) {
    df <- copy_activity_column(df, target, aliases[[target]])
  }

  numeric_columns <- setdiff(activity_columns, "timestamp")
  for (column in numeric_columns) {
    if (!is.numeric(df[[column]])) {
      df[[column]] <- suppressWarnings(as.numeric(df[[column]]))
    }
  }

  lat_values <- df$position_lat[is.finite(df$position_lat)]
  lon_values <- df$position_long[is.finite(df$position_long)]

  if (length(lat_values) > 0L && any(abs(lat_values) > 90)) {
    df$position_lat <- semicircles_to_degrees(df$position_lat)
  }

  if (length(lon_values) > 0L && any(abs(lon_values) > 180)) {
    df$position_long <- semicircles_to_degrees(df$position_long)
  }

  df[c(activity_columns, setdiff(names(df), activity_columns))]
}

ensure_activity_columns <- function(df) {
  missing_columns <- setdiff(activity_columns, names(df))
  for (column in missing_columns) {
    df[[column]] <- NA
  }

  df[c(activity_columns, setdiff(names(df), activity_columns))]
}

read_fit_activity <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("Nie znaleziono wskazanego pliku FIT.", call. = FALSE)
  }

  if (!requireNamespace("FITfileR", quietly = TRUE)) {
    stop(
      paste(
        "Do importu plików FIT zainstaluj pakiet FITfileR:",
        "remotes::install_github('grimbough/FITfileR')."
      ),
      call. = FALSE
    )
  }

  fit_file <- tryCatch(
    FITfileR::readFitFile(path),
    error = function(error) {
      stop(
        paste("Nie udało się odczytać pliku FIT:", conditionMessage(error)),
        call. = FALSE
      )
    }
  )

  record_sets <- tryCatch(
    FITfileR::records(fit_file),
    error = function(error) {
      stop(
        paste("Nie udało się wyodrębnić rekordów aktywności:", conditionMessage(error)),
        call. = FALSE
      )
    }
  )

  records <- combine_fit_records(record_sets)
  if (nrow(records) == 0L) {
    stop("Plik FIT nie zawiera rekordów aktywności.", call. = FALSE)
  }

  ensure_activity_columns(standardize_fit_columns(records))
}
