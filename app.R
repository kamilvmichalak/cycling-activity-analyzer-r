library(shiny)

source("R/import_fit.R", encoding = "UTF-8")
source("R/prepare_data.R", encoding = "UTF-8")
source("R/summary.R", encoding = "UTF-8")
source("R/plots.R", encoding = "UTF-8")
source("R/segments.R", encoding = "UTF-8")
source("R/heart_zones.R", encoding = "UTF-8")

ui <- fluidPage(
  titlePanel("Analizator aktywności rowerowych"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "fit_file",
        label = "Wybierz plik aktywności .fit",
        accept = c(".fit", ".FIT")
      ),
      uiOutput("file_status"),
      hr(),
      selectInput(
        "segment_by",
        "Segmentacja",
        choices = c("Dystans" = "distance", "Czas" = "time")
      ),
      uiOutput("segment_length_control"),
      hr(),
      numericInput(
        "max_hr",
        "Tętno maksymalne",
        value = 190,
        min = 100,
        max = 230,
        step = 1
      )
    ),
    mainPanel(
      fluidRow(
        column(
          width = 8,
          tabsetPanel(
            tabPanel(
              "Podsumowanie",
              h3("Import danych"),
              uiOutput("import_status"),
              textOutput("record_count"),
              h3("Podsumowanie aktywności"),
              tableOutput("activity_summary")
            ),
            tabPanel(
              "Dane",
              h3("Informacje o pliku"),
              verbatimTextOutput("file_info"),
              h4("Dostępne kolumny"),
              verbatimTextOutput("column_names"),
              h4("Podgląd danych"),
              tableOutput("data_preview")
            ),
            tabPanel(
              "Wykresy",
              plotOutput("speed_plot", height = "300px"),
              plotOutput("heart_rate_plot", height = "300px"),
              plotOutput("altitude_plot", height = "300px"),
              plotOutput("route_plot", height = "420px")
            ),
            tabPanel(
              "Segmenty",
              h3("Analiza segmentów"),
              tableOutput("segments_table"),
              plotOutput("segments_plot", height = "360px")
            ),
            tabPanel(
              "Strefy tętna",
              h3("Strefy tętna"),
              uiOutput("heart_zones_status"),
              tableOutput("heart_zones_table"),
              plotOutput("heart_zones_plot", height = "360px")
            )
          )
        ),
        column(
          width = 4,
          wellPanel(
            h3("Mapa trasy"),
            uiOutput("map_status"),
            leaflet::leafletOutput("route_map", height = "520px")
          )
        )
      )
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

  activity_summary <- reactive({
    summarise_activity(activity_data())
  })

  output$segment_length_control <- renderUI({
    if (identical(input$segment_by, "time")) {
      numericInput(
        "segment_length",
        "Długość segmentu [min]",
        value = 5,
        min = 0.5,
        step = 0.5
      )
    } else {
      numericInput(
        "segment_length",
        "Długość segmentu [km]",
        value = 1,
        min = 0.1,
        step = 0.1
      )
    }
  })

  activity_segments <- reactive({
    req(input$segment_length)
    create_activity_segments(
      activity_data(),
      by = input$segment_by,
      segment_length = input$segment_length
    )
  })

  heart_rate_zones <- reactive({
    calculate_heart_rate_zones(activity_data(), max_hr = input$max_hr)
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

  output$activity_summary <- renderTable({
    summary_as_data_frame(activity_summary())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$speed_plot <- renderPlot({
    plot_speed(activity_data())
  })

  output$heart_rate_plot <- renderPlot({
    plot_heart_rate(activity_data())
  })

  output$altitude_plot <- renderPlot({
    plot_altitude(activity_data())
  })

  output$route_plot <- renderPlot({
    plot_route(activity_data())
  })

  output$map_status <- renderUI({
    if (is.null(input$fit_file)) {
      return(tags$p("Mapa pojawi się po wczytaniu pliku FIT."))
    }

    if (nrow(route_data(activity_data())) < 2L) {
      return(tags$div(class = "alert alert-warning", "Brak danych GPS w aktywności."))
    }

    tags$p("Interaktywny ślad GPS aktywności.")
  })

  output$route_map <- leaflet::renderLeaflet({
    data <- activity_data()
    validate(need(nrow(route_data(data)) >= 2L, "Brak danych GPS."))
    create_route_map(data)
  })

  output$segments_table <- renderTable({
    segments_as_data_frame(activity_segments())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$segments_plot <- renderPlot({
    plot_segments_speed(activity_segments())
  })

  output$heart_zones_status <- renderUI({
    if (nrow(heart_rate_zones()) == 0L) {
      return(tags$div(
        class = "alert alert-warning",
        "Brak danych tętna w aktywności."
      ))
    }

    tags$p("Strefy wyliczono na podstawie podanego tętna maksymalnego.")
  })

  output$heart_zones_table <- renderTable({
    heart_zones_as_data_frame(heart_rate_zones())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$heart_zones_plot <- renderPlot({
    plot_heart_rate_zones(heart_rate_zones())
  })
}

shinyApp(ui = ui, server = server)
