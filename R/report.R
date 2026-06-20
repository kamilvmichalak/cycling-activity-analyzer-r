generate_activity_report <- function(
  df,
  output_file,
  max_hr = 190,
  params = list()
) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop("Brak danych do wygenerowania raportu.", call. = FALSE)
  }

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Do generowania raportu zainstaluj pakiet rmarkdown.", call. = FALSE)
  }

  template <- file.path("reports", "activity_report.Rmd")
  if (!file.exists(template)) {
    stop("Nie znaleziono szablonu reports/activity_report.Rmd.", call. = FALSE)
  }

  report_params <- utils::modifyList(
    list(activity_data = df, max_hr = max_hr),
    params
  )

  render_directory <- tempfile("cycling-activity-report-")
  dir.create(render_directory, recursive = TRUE)
  on.exit(unlink(render_directory, recursive = TRUE, force = TRUE), add = TRUE)

  rendered_file <- rmarkdown::render(
    input = normalizePath(template, mustWork = TRUE),
    output_file = "raport-aktywnosci.html",
    output_dir = render_directory,
    params = report_params,
    envir = new.env(parent = globalenv()),
    quiet = TRUE,
    encoding = "UTF-8"
  )

  if (!file.copy(rendered_file, output_file, overwrite = TRUE)) {
    stop("Nie udało się zapisać wygenerowanego raportu.", call. = FALSE)
  }

  invisible(output_file)
}
