library(shiny)

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
      verbatimTextOutput("file_info")
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
}

shinyApp(ui = ui, server = server)
