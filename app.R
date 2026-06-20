library(shiny)

source("R/import_fit.R", encoding = "UTF-8")
source("R/prepare_data.R", encoding = "UTF-8")

ui <- fluidPage(
  titlePanel("Analizator aktywności rowerowych"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "fit_file",
        label = "Wybierz plik aktywności .fit",
        accept = c(".fit", ".FIT")
      ),
      uiOutput("file_status")
    ),
    mainPanel(
      h3("Informacje o pliku"),
      verbatimTextOutput("file_info"),
      hr(),
      h3("Import danych"),
      uiOutput("import_status"),
      textOutput("record_count"),
      h4("Dostępne kolumny"),
      verbatimTextOutput("column_names"),
      h4("Podgląd danych"),
      tableOutput("data_preview")
    )
  )
)

server <- function(input, output, session) {
  file_validation <- reactive({
    file <- input$fit_file

    if (is.null(file)) {
      return(list(
        valid = FALSE,
        message = "Wybierz plik FIT, aby rozpocząć analizę."
      ))
    }

    if (tolower(tools::file_ext(file$name)) != "fit") {
      return(list(
        valid = FALSE,
        message = "Nieprawidłowy format. Wybierz plik z rozszerzeniem .fit."
      ))
    }

    list(valid = TRUE, message = "Plik FIT został wybrany poprawnie.")
  })

  output$file_status <- renderUI({
    validation <- file_validation()
    status_class <- if (validation$valid) {
      "alert alert-success"
    } else if (is.null(input$fit_file)) {
      "alert alert-info"
    } else {
      "alert alert-danger"
    }

    tags$div(class = status_class, validation$message)
  })

  output$file_info <- renderPrint({
    file <- input$fit_file

    validate(
      need(!is.null(file), "Brak wybranego pliku."),
      need(file_validation()$valid, file_validation()$message)
    )

    data.frame(
      informacja = c("Nazwa", "Rozmiar [B]", "Typ MIME", "Ścieżka tymczasowa"),
      wartosc = c(
        file$name,
        file$size,
        ifelse(is.na(file$type) || file$type == "", "nieznany", file$type),
        file$datapath
      ),
      check.names = FALSE
    )
  })

  activity_import <- reactive({
    req(input$fit_file)
    req(file_validation()$valid)

    tryCatch(
      list(
        data = read_fit_activity(input$fit_file$datapath),
        error = NULL
      ),
      error = function(error) {
        list(data = NULL, error = conditionMessage(error))
      }
    )
  })

  activity_raw <- reactive({
    result <- activity_import()
    validate(need(is.null(result$error), result$error))
    result$data
  })

  activity_data <- reactive({
    prepare_activity_data(activity_raw())
  })

  output$import_status <- renderUI({
    if (is.null(input$fit_file) || !file_validation()$valid) {
      return(tags$div(
        class = "alert alert-info",
        "Dane zostaną wczytane po wybraniu poprawnego pliku FIT."
      ))
    }

    result <- activity_import()
    if (!is.null(result$error)) {
      return(tags$div(class = "alert alert-danger", result$error))
    }

    tags$div(class = "alert alert-success", "Dane zostały wczytane do data.frame.")
  })

  output$record_count <- renderText({
    paste(
      "Liczba rekordów po imporcie:", nrow(activity_raw()),
      "| po przygotowaniu:", nrow(activity_data())
    )
  })

  output$column_names <- renderPrint({
    names(activity_data())
  })

  output$data_preview <- renderTable({
    utils::head(activity_data(), 10L)
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui = ui, server = server)
