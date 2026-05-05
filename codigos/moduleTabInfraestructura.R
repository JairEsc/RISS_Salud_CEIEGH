##Módulo de infraestructura/ capacidad instalada/ equipamiento / etc.
##Se cuenta con dos fuentes de información: 
###Catalogo_k: lista de equipamientos que debería tener un clues de nivel k.
###equipamiento_k: lista de clues que cuentan con (bool) el equipamiento 

#Prototipo
#El $p%$ de las clues $N_i$ cuentan con $equipamiento_1$^{x} 
#y $equipamiento_2$^{x} (+)
#...
# X1 |  X2  |  X3  |  X4  |
#map
equipamiento_opciones=colnames(sinerhias_N1)[2:80]
equipamiento_default <- sample(equipamiento_opciones,size = 1)

tabInfraUI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = "infra",
    uiOutput(ns("frase")),
    uiOutput(ns("equipamiento_list")),
    leafletOutput(ns("equipamiento"), height = "55vh")
  )
}

tabInfraServer <- function(id, nivel_at, clues_en_operacion) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      porcentaje = reactiveVal(value = 0)
      equip_inputs <- reactiveVal(c("equip_1"))#Lista de equipamientos seleccionados
      input_counter <- reactiveVal(1)#Número de equipamientos consultados

      get_selected_equipamientos <- reactive({
        ids <- equip_inputs()
        if (length(ids) == 0) {
          return(equipamiento_default)#Azar
        }
        vapply(ids, function(id) {
          value <- input[[id]]
          if (is.null(value) || value == "") {
            sample(equipamiento_opciones,size = 1)##Azar forzado
          } else {
            value
          }
        }, character(1), USE.NAMES = FALSE)
      })

      observeEvent(input$add_equipamiento, {#Botoncito de agregar otro equipamiento
        next_id <- paste0("equip_", input_counter() + 1)
        input_counter(input_counter() + 1)
        equip_inputs(c(equip_inputs(), next_id))
      })

      observe({##Creamos los eventos para eliminar inputs.  
        ids <- equip_inputs()
        lapply(ids, function(id) {
          remove_id <- paste0("remove_", id)
          observeEvent(input[[remove_id]], {
            equip_inputs(setdiff(equip_inputs(), id))
          }, ignoreInit = TRUE, once = TRUE)
        })
      })

      output$frase <- renderUI({
        tags$p(
          paste0("El ", porcentaje(), "% de los CLUES de ", stringr::str_to_lower(nivel_at()), " ", "cuentan con :"),
          class = "infra-frase"
        )
      })

      output$equipamiento_list <- renderUI({
        ids <- equip_inputs()
        if (length(ids) == 0) return(NULL)
        tagList(
          lapply(seq_along(ids), function(i) {
            id <- ids[i]
            selected <- input[[id]]
            if (is.null(selected) || selected == "") {
              selected <- equipamiento_default
            }
            div(
              class = "infra-input-card",
              selectInput(ns(id), label = NULL, choices = equipamiento_opciones, selected = selected),
              if (length(ids) > 1) {##Si hay más de uno, podemos eliminarlos
                actionButton(ns(paste0("remove_", id)), label = HTML("&times;"), class = "btn btn-link remove-input")
              },
              if (i == length(ids)) {##La opción de agregar en la último select
                actionButton(ns("add_equipamiento"), label = HTML("+"), class = "btn btn-default btn-sm infra-add-input")
              }
            )
          })
        )
      })

      output$equipamiento <- renderLeaflet({
        selected <- unique(get_selected_equipamientos())##Opciones seleccionadas únicas. 
        datas <- if (length(selected) == 0) {##Nunca serían cero.
          sinerhias_N1 |> dplyr::select(CLUES) |> dplyr::collect() 
        } else {
          sinerhias_N1 |>
            dplyr::filter(dplyr::if_all(dplyr::all_of(selected), ~ . == 1)) |>
            dplyr::select(CLUES) |> 
            dplyr::collect() 
        }
        porcentaje(round(100 * length(datas$CLUES) / (sinerhias_N1 |> dplyr::count() |> dplyr::collect()), 2))
        clues_con_equipam <- clues_en_operacion |>
          dplyr::filter(CLUES %in% datas$CLUES) |>
          dplyr::select(CLUES, geometry) |>
          dplyr::collect() |>
          dplyr::mutate(geometry = sf::st_as_sfc(structure(geometry, class = "WKB"), EWKB = T)) |>
          st_as_sf()
        leaflet() |> addTiles() |> addMarkers(data = clues_con_equipam)
      })
    }
  )
}


