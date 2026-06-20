library(shiny)

ui <- fluidPage(
  titlePanel("Analizator aktywności rowerowych"),
  sidebarLayout(
    sidebarPanel(
      p(
        paste(
          "W kolejnych etapach dodany zostanie import plików FIT",
          "oraz analiza aktywności."
        )
      )
    ),
    mainPanel(
      h3("Panel wyników"),
      p("Brak wczytanej aktywności.")
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
