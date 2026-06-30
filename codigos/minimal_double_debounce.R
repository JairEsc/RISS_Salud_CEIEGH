library(shiny)
library(shinybusy)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(sf)
library(raster)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(rintrojs)
library(dbplyr)
library(archive)
library(shinyalert)

# Placeholders to prevent crashes
leaflet_legend_css <- ""
sidebar_last_child_css <- ""
tour_button_css <- ""

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Visualizador", disable = FALSE),
  
  shinydashboardPlus::dashboardSidebar(
    tags$head(
      tags$style(HTML(leaflet_legend_css)),
      tags$style(HTML(sidebar_last_child_css)),
      tags$style(HTML(tour_button_css)),
      tags$style(HTML("
        .sweet-alert h2 { font-size: 20px !important; margin: 10px 0 !important; }
        .sweet-alert { width: 350px !important; padding: 15px !important; left: 50% !important; margin-left: -175px !important; }
        .sweet-alert .lead { font-size: 14px !important; }
        .shinyalert-checkboxes { text-align: left; margin: 10px auto; width: fit-content; }
      "))
    ),
    
    div(class = "sidebar-controls",
        introBox(id = "tour_step_2_nivel", data.step = 1, data.intro = "placeholder",
                 div(style='display:flex; gap: 5px; padding: 10px;',
                     selectInput("nivel_at",
                                 label = "Nivel de atención", 
                                 choices = c("1er nivel" = "PRIMER NIVEL",
                                             "2do nivel" = "SEGUNDO NIVEL",
                                             "3er nivel" = "TERCER NIVEL",
                                             "Todos los niveles"='CUALQUIER NIVEL'),
                                 selectize = TRUE, selected ="SEGUNDO NIVEL" ),
                     actionButton("filtrarPublicoPrivado", class="btn-primary", icon = icon("filter"), label = "", style="margin-top: 24px;")
                 )
        )
    ),
    shinyjs::useShinyjs(),
    collapsed = FALSE, minified = FALSE
  ),
  
  dashboardBody(
    fluidRow(
      box(title = "Debounce & Gatekeeper Debugging Monitor", width = 12, status = "primary",
          verbatimTextOutput("debug_status")
      )
    )
  )
)

shinyApp(ui, function(input, output, session) {
  
  # State tracking
  rv <- reactiveValues(
    publicos = TRUE, 
    privados = TRUE,
    modal_open = FALSE # The gatekeeper flag
  )
  
  # Trigger modal and lock calculations
  observeEvent(input$filtrarPublicoPrivado, {
    rv$modal_open <- TRUE # Freeze updates!
    
    shinyalert(
      html = TRUE,
      text = tagList(
        div(class = "shinyalert-checkboxes",
            checkboxInput("publicas_", label = "Públicas", value = rv$publicos),
            checkboxInput("privadas_", label = "Privadas", value = rv$privados)
        )
      ),
      callbackR = function(value) {
        # This runs whether they click Aceptar (TRUE) or Cancel/Outside (FALSE)
        if(isTRUE(value)) { 
          rv$publicos <- input$publicas_
          rv$privados <- input$privadas_
        }
        # Unfreeze updates only after modal resolution
        rv$modal_open <- FALSE 
      },
      closeOnClickOutside = TRUE,
      title = "Selecciona el tipo de CLUES",
      confirmButtonText = "Aceptar"
    )
  })
  
  # Intermediate reactive: Only passes values downstream if the modal is CLOSED
  # isolate() prevents input$nivel_at from breaking out of the gate prematurely
  gatekeeper_inputs <- reactive({
    if (rv$modal_open) {
      req(FALSE) # Silently halt execution while modal is open
    }
    
    list(
      nivel = input$nivel_at,
      publicos = rv$publicos,
      privados = rv$privados
    )
  })
  
  # Apply debounce to the gated inputs
  debounced_inputs <- gatekeeper_inputs |> debounce(1000)
  
  # Render text for verification
  output$debug_status <- renderText({
    current_filters <- debounced_inputs()
    
    tipo_filtro <- c()
    if(current_filters$publicos) tipo_filtro <- c(tipo_filtro, "Público")
    if(current_filters$privados) tipo_filtro <- c(tipo_filtro, "Privado")
    
    paste0(
      "[TIMESTAMP]: ", Sys.time(), "\n",
      "-----------------------------------------\n",
      "Modal Currently Open?: ", rv$modal_open, "\n\n",
      "Selected Nivel: ", current_filters$nivel, "\n",
      "Include Públicas: ", current_filters$publicos, "\n",
      "Include Privadas: ", current_filters$privados, "\n\n",
      "Simulated dplyr::filter Code:\n",
      "data %>% \n",
      "  filter(nivel_atencion == '", current_filters$nivel, "') %>% \n",
      "  filter(archivo_origen %in% c(", paste(sprintf("'%s'", tipo_filtro), collapse = ", "), "))"
    )
  })
})