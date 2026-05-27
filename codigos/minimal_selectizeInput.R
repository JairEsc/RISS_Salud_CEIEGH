library(shiny)

my_autocomplete_list <- c("John Doe","Ash","Ajay sharma",
                          "Ken Chong","Will Smith","Neo")

ui <- fluidPage(
  titlePanel("Minimal selectizeInput demo"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = 'search',
        label = 'Search',
        choices = my_autocomplete_list,
        selected = my_autocomplete_list[1],
        multiple = FALSE,
        options = list(create = FALSE, placeholder = 'Type or select an option...')
      )
    ),
    mainPanel(
      h4("Last confirmed option"),
      verbatimTextOutput("confirmed_value"),
      h4("Raw input state"),
      verbatimTextOutput("raw_state"),
      tags$p("The confirmed option updates automatically when you select an item from the dropdown.")
    )
  )
)

server <- function(input, output, session) {
  confirmed <- reactiveVal(my_autocomplete_list[1])

  observeEvent(input$search, {
    if (!is.null(input$search) && input$search != "") {
      confirmed(input$search)
    }
    ## If raw input is empty, do not change confirmed yet.
  })

  output$confirmed_value <- renderText({
    confirmed()
  })

  output$raw_state <- renderPrint({
    str(input$search)
  })
}

shinyApp(ui = ui, server = server)