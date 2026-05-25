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

tabInfraUI <- function(id){
  ns <- NS(id)
  tabItem(
    tabName = "infra",
    uiOutput(ns("frase")),
    uiOutput(ns("equipamiento_list")),
    leafletOutput(ns("equipamiento"), height = "55vh")
  )
}

tabInfraServer <- function(id, nivel_at,clues_en_operacion) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      porcentaje = reactiveVal(value = 0)
      equip_inputs <- reactiveVal(c("equip_1"))#Lista de equipamientos seleccionados
      input_counter <- reactiveVal(1)#Número de equipamientos consultados
      equip_defaults <- reactiveVal(list())#Store default values for new inputs
      output$equipamiento <- renderLeaflet({
        leaflet() |> addTiles() 
      })
      equipamiento_opciones=reactive({
        #req(nivel_at())
        print(nivel_at())
        nivel_atencion=switch(nivel_at(),
                              "PRIMER NIVEL" = {
                                "N1_"
                              },
                              "SEGUNDO NIVEL" = {
                                "N2_"
                              },
                              "TERCER NIVEL" = {
                                "N3_"
                              },
                              "CUALQUIER NIVEL" = {
                                ""
                              }
        )
        cols <- dplyr::tbl(sinerhias,paste0(nivel_atencion, "CLUES_SINERHIAS")) |>
          colnames()
        print(cols)
        lista_opciones=list()
        lista_opciones[['catalogo']]=cols[2:(length(cols)-1)]##Remove las que no sean infrarestuctura.
        lista_opciones[['tabla']]=dplyr::tbl(sinerhias,paste0(nivel_atencion, "CLUES_SINERHIAS"))
        lista_opciones
      })
      equipamiento_default=reactive({
        eleccion=sample(equipamiento_opciones()[['catalogo']],size = 1)
        eleccion
      })
      sinerhias_nivel_actual=reactive({
        equipamiento_opciones()[['tabla']]
      })
      
      ##Observe nivel_at changes to reset equipment selections
      observeEvent(nivel_at(), {
        equip_inputs(c("equip_1"))
        input_counter(1)
        equip_defaults(list())##Clear stored defaults
      })
      
      ##Map update function
      update_mapa <- function() {
        print("updated")
        ids <- equip_inputs()
        selected <- unique(na.omit(c(sapply(ids, function(x) {
          val <- input[[x]]
          if (is.null(val) || val == "") NA else val
        }))))
        
        ##Skip if no selections made yet
        if (length(selected) == 0) {
          return(invisible(NULL))
        }
        
        datas <- tryCatch({
          sinerhias_nivel_actual() |>
            dplyr::filter(dplyr::if_all(dplyr::all_of(selected), ~ . == 1)) |>
            dplyr::select(CLUES) |>
            dplyr::collect()
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(datas)) return(invisible(NULL))
        
        porcentaje(round(100 * length(datas$CLUES) / (sinerhias_nivel_actual() |> dplyr::count() |> dplyr::collect()), 2))
        clues_con_equipam <- clues_en_operacion |>
          dplyr::filter(CLUES %in% datas$CLUES) |>
          dplyr::select(CLUES, geometry) |>
          dplyr::collect() |>
          dplyr::mutate(geometry = sf::st_as_sfc(structure(geometry, class = "WKB"), EWKB = T)) |>
          st_as_sf()
        if(nrow(clues_con_equipam)>0){
          res_raster <- gdistance::accCost(T.GC, matrix(unlist(clues_con_equipam |> st_transform(32614) |> st_geometry()),nrow = nrow(clues_con_equipam),ncol = 2,byrow = T))
          crs(res_raster)=st_crs("EPSG:32614")$wkt
          res_raster[res_raster>90]=NA
          leafletProxy("equipamiento")|> 
            clearImages() |> 
            addRasterImage(projectRasterForLeaflet(res_raster,method = "ngb"),colors = "Spectral",group = "Accesibilidad peatonal (en minutos)")|>
            clearMarkers() |> 
            addMarkers(data=clues_con_equipam,layerId = clues_con_equipam$CLUES)
        }
        else{
          leafletProxy("equipamiento")|> 
            clearImages() |> 
            clearMarkers()
        }
      }

      observeEvent(input$add_equipamiento, {#Botoncito de agregar otro equipamiento
        next_id <- paste0("equip_", input_counter() + 1)
        ##Use previous value as default for new input
        prev_value <- input[[paste0("equip_", input_counter())]]
        input_counter(input_counter() + 1)
        equip_inputs(c(equip_inputs(), next_id))
        ##Store default value for this new input (if previous had a value)
        if (!is.null(prev_value) && prev_value != "") {
          new_defaults <- equip_defaults()
          new_defaults[[next_id]] <- prev_value
          equip_defaults(new_defaults)
        }
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
              ##Check if we have a stored default for this input
              if (id %in% names(equip_defaults())) {
                selected <- equip_defaults()[[id]]
              } else {
                selected <- equipamiento_default()
              }
            }
            div(
              class = "infra-input-card",
              selectInput(ns(id), label = NULL, choices = equipamiento_opciones()[['catalogo']], selected = selected),
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
      
      ##Store last known selected values to detect actual changes
      last_selected_values <- reactiveVal(character(0))
      
      ##Observe nivel_at changes
      observeEvent(nivel_at(), {
        update_mapa()
      }, ignoreInit = TRUE)
      
      ##Observe input changes (all current input selectors)
      observe({
        ids <- equip_inputs()
        ##Extract current values WITHOUT depending on equip_inputs list structure
        current_values <- unique(na.omit(c(sapply(ids, function(x) {
          val <- input[[x]]
          if (is.null(val) || val == "") NA else val
        }))))
        
        ##Only update map if the actual selected VALUES changed
        if (!identical(sort(current_values), sort(last_selected_values()))) {
          last_selected_values(current_values)
          update_mapa()
        }
      }, label = "equipment-values-watcher")
      # output$equipamiento <- renderLeaflet({##Pendiente. Accesibilidad y pop-ups/labels
      #   selected <- unique(get_selected_equipamientos())##Opciones seleccionadas únicas. 
      #   datas <- if (length(selected) == 0) {##Nunca serían cero.
      #     sinerhias_N1 |> dplyr::select(CLUES) |> dplyr::collect() 
      #   } else {
      #     sinerhias_N1 |>
      #       dplyr::filter(dplyr::if_all(dplyr::all_of(selected), ~ . == 1)) |>
      #       dplyr::select(CLUES) |> 
      #       dplyr::collect() 
      #   }
      #   porcentaje(round(100 * length(datas$CLUES) / (sinerhias_N1 |> dplyr::count() |> dplyr::collect()), 2))
      #   clues_con_equipam <- clues_en_operacion |>
      #     dplyr::filter(CLUES %in% datas$CLUES) |>
      #     dplyr::select(CLUES, geometry) |>
      #     dplyr::collect() |>
      #     dplyr::mutate(geometry = sf::st_as_sfc(structure(geometry, class = "WKB"), EWKB = T)) |>
      #     st_as_sf()
      #   ##Isocronas(clues_con_equipam)
      #   ##Demografia(isocronas)
      #   ##lógica para agregar al mapa
      #   leaflet() |> addTiles() |> addMarkers(data = clues_con_equipam)
      # })
      # observeEvent(input$equipamiento_marker_click,{
      #   print(input$equipamiento_marker_click)
      #   punto_referencia_fijo=st_point(c(input$equipamiento_marker_click$lng ,input$equipamiento_marker_click$lat)) |> st_sfc(crs = 4326)
      #   res_raster <- gdistance::accCost(T.GC, punto_referencia_fijo |> st_transform(st_crs("EPSG:32614")) |> unlist())
      #   crs(res_raster)=st_crs("EPSG:32614")$wkt
      #   
      #   AccesibilidadCLUES(poligono =clues_en_operacion |>
      #                        dplyr::filter(CLUES == input$equipamiento_marker_click$id) |>
      #                        dplyr::collect() ,
      #   centro=punto_referencia_fijo,
      #   leaflet_proxy = "equipamiento")
      #   leafletProxy("equipamiento")|> 
      #     clearImages() |> 
      #     addRasterImage(projectRasterForLeaflet(res_raster,method = "ngb"),colors = "Spectral",group = "Accesibilidad peatonal (en minutos)")  
      #     
      # })
    }
  )
}


