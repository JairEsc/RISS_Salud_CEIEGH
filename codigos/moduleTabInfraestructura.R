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
    introBox(id = "tour_step_7_infra",data.step = 7,
             data.intro = "",
    uiOutput(ns("frase")),
    uiOutput(ns("equipamiento_list"))),
    leafletOutput(ns("equipamiento"), height = "65vh"),
    shiny::actionButton(ns("calcular_accesibilidad"),label = "Calcular Accesibilidad")
  )
}

tabInfraServer <- function(id, nivel_at, selected_tab, clues_en_operacion, sinerhias) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      porcentaje = reactiveVal(value = 0)
      clues_con_equipamiento = reactiveVal(value = as.data.frame(0) )
      equip_inputs <- reactiveVal(c("equip_1"))#Lista de equipamientos seleccionados
      input_counter <- reactiveVal(1)#Número de equipamientos consultados
      equip_defaults <- reactiveVal(list())#Store default values for new inputs
      inputs_initialized <- reactiveVal(character(0))#Track first initialization of select inputs
      output$equipamiento <- renderLeaflet({
        leaflet() |> addTiles() |> 
          setView(lng = -98.83284,lat = 20.45979,zoom = 9) |> 
          addLegend_custom(legendCustom = "clues")
      })

      ## Helper: snapshot current input values into equip_defaults
      snapshot_current_inputs <- function() {
        ids <- equip_inputs()
        if (length(ids) == 0) return(invisible(NULL))
        cur <- equip_defaults()
        for (id in ids) {
          val <- input[[id]]
          if (!is.null(val) && val != "") {
            cur[[id]] <- val
          }
        }
        equip_defaults(cur)
        invisible(NULL)
      }
      equipamiento_opciones=reactive({
        req(selected_tab() == "infra")
        #print(nivel_at())
        
        ##
        nivel_col <- switch(nivel_at(),
          "PRIMER NIVEL" = "primer_nivel",
          "SEGUNDO NIVEL" = "segundo_nivel",
          "TERCER NIVEL" = "tercer_nivel",
          "CUALQUIER NIVEL" = "cualquier_nivel"
        )
        
        ##Get the tabla name for queries
        nivel_atencion <- switch(nivel_at(),
          "PRIMER NIVEL" = "N1_",
          "SEGUNDO NIVEL" = "N2_",
          "TERCER NIVEL" = "N3_",
          "CUALQUIER NIVEL" = ""
        )
        
        ##Catalogo
        catalog <- dplyr::tbl(sinerhias, "catalogo") |>
          dplyr::select(NombreVar, Descripcion.de.la.variable, !!nivel_col,!!paste0("disponibilidad_",nivel_col)) |>
          dplyr::filter((!!dplyr::sym(nivel_col))>0) |>
          dplyr::collect() |> 
          dplyr::arrange(dplyr::desc(!!dplyr::sym(paste0("disponibilidad_",nivel_col) )))
        

        lista_opciones <- list()
        ##
        lista_opciones[['catalogo']] <- setNames(
          catalog$NombreVar,
          catalog$Descripcion.de.la.variable
        )
        #print(lista_opciones[['catalogo']])
        ##conexion a la tabla de clues
        lista_opciones[['tabla']] <- dplyr::tbl(sinerhias, paste0(nivel_atencion, "CLUES_SINERHIAS"))
        
        lista_opciones
      })
      sinerhias_nivel_actual=reactive({
        equipamiento_opciones()[['tabla']]
      })
      ##Observe nivel_at changes to reset equipment selections
      observeEvent(nivel_at(), {
        ##Pendiente: Pasar de 1->2 o 2->3 mantiene las opciones seleccionadas. 
        equip_inputs(c("equip_1"))
        input_counter(1)
        equip_defaults(list())##Clear stored defaults
      })
      ##Map update function
      update_mapa <- function() {
        req(selected_tab() == "infra")
        #print("updated")
        ids <- equip_inputs()## puede tener la forma c("equip_1","equip_5","equip_7")
        #print(ids)
        selected <- unique(na.omit(c(sapply(ids, function(x) {
          val <- input[[x]]
          if (is.null(val) || val == "") NA else val
        }))))
        cat("Estas son las opciones distintas de la consulta actual") ##
        cat(selected) ##Estas son las opciones distintas de la consulta actual

        consulta_actual <- tryCatch({
          sinerhias_nivel_actual() |>
            dplyr::filter(dplyr::if_all(dplyr::all_of(selected), ~ . == 1)) |>
            dplyr::select(CLUES,NIVEL.ATENCION,NOMBRE.DE.LA.INSTITUCION,MUNICIPIO,LOCALIDAD,NOMBRE.DE.LA.UNIDAD,equipamiento,geometry) |>
            dplyr::collect()
        }, error = function(e) {
          return(NULL)
        })##Consulta de las CLUES
        
        if (is.null(consulta_actual)) return(invisible(NULL))
        ##Actualizamos la frase
        # print("Sinerhias nivel actual:")
        # print(sinerhias_nivel_actual() |> dplyr::count() |> dplyr::collect())
        # print("Consulta actual:")
        # print(consulta_actual |> nrow())
        porcentaje(round(100 * nrow(consulta_actual) / (sinerhias_nivel_actual() |> dplyr::count() |> dplyr::collect()), 2))
        clues_con_equipam <- consulta_actual |> 
          dplyr::mutate(geometry = sf::st_as_sfc(structure(geometry, class = "WKB"), EWKB = T)) |>
          st_as_sf()
        clues_con_equipamiento(clues_con_equipam)
        leafletProxy("equipamiento")|> 
          clearImages() |> 
          clearMarkers() |> 
          clearGroup("CLUES") |> 
          removeControl(layerId = "Accesibilidad en minutos2")
          
        
        if(nrow(clues_con_equipam)>0){##Solo actualizmos si hay clues con el equipamiento descrito
          popups_list <- apply(clues_con_equipam, 1, function(row) {
            generadorPopUpContentCLUESInfra(as.list(row),opciones_catalogo=length(equipamiento_opciones()[['catalogo']]) )
          })
          leafletProxy("equipamiento")|> 
            addMarkers_custom(data =clues_con_equipam,addSearch = F ,
                              popups=popups_list
                              )
        }
      }   
      observeEvent(input$calcular_accesibilidad,{
        req(selected_tab() == "infra")
        clues_con_equipam=clues_con_equipamiento()
        if(nrow(clues_con_equipam)==0){
          showNotification(paste0(nrow(clues_solicitados)," CLUES cuentan con este equipamiento"))
        }
        req(nrow(clues_con_equipam)>0)
        res_raster <- gdistance::accCost(T.GC, matrix(unlist(clues_con_equipam |> st_transform(32614) |> st_geometry()),nrow = nrow(clues_con_equipam),ncol = 2,byrow = T))
        crs(res_raster)=st_crs("EPSG:32614")$wkt
        res_raster[res_raster>90]=NA
        ##Pendiente: Colores de markers dependiendo nivel de atencion. 
        leafletProxy("equipamiento")|> 
          addRasterImage(projectRasterForLeaflet(res_raster,method = "ngb"),colors = "Spectral",group = "Accesibilidad peatonal (en minutos)")|> 
          addLegend_custom(legendCustom = "raster") 
      })
      observeEvent(input$add_equipamiento, {#Botoncito de agregar otro equipamiento
        snapshot_current_inputs()
        prev_ids <- equip_inputs()
        prev_id <- if (length(prev_ids) > 0) prev_ids[length(prev_ids)] else NULL
        prev_val <- NULL
        if (!is.null(prev_id)) {
          cur_defaults <- equip_defaults()
          if (!is.null(cur_defaults[[prev_id]])) prev_val <- cur_defaults[[prev_id]]
        }
        next_id <- paste0("equip_", input_counter() + 1)
        input_counter(input_counter() + 1)
        equip_inputs(c(equip_inputs(), next_id))
        ##Set default for the new input to the previous value (if any)
        new_defaults <- equip_defaults()
        if (!is.null(prev_val)) new_defaults[[next_id]] <- prev_val
        equip_defaults(new_defaults)
      })

      observe({##Creamos los eventos para eliminar inputs.  
        ids <- equip_inputs()##Cuando este se actualiza, creamos eventos para escuchar a estos ID's pero para borrar
        lapply(ids, function(id) {
          remove_id <- paste0("remove_", id)
          observeEvent(input[[remove_id]], {
            ##Snapshot before removing so we preserve other selections
            snapshot_current_inputs()
            equip_inputs(setdiff(equip_inputs(), id))
            new_defaults <- equip_defaults()
            new_defaults[[id]] <- NULL
            equip_defaults(new_defaults)
            inputs_initialized(setdiff(inputs_initialized(), id))
          }, ignoreInit = TRUE, once = TRUE)
        })
      })

      output$frase <- renderUI({
        nrow_nivel <- switch(nivel_at(),
                            "PRIMER NIVEL" = 833,
                            "SEGUNDO NIVEL" = 32,
                            "TERCER NIVEL" = 2,
                            "CUALQUIER NIVEL" = 867
        )
        
        tags$p(
          paste0(
            "De los ",nrow_nivel," CLUES de ",stringr::str_to_lower(nivel_at()),", ", nrow(clues_con_equipamiento()), " CLUES (",porcentaje(),"%) cuentan
            con cada uno de los siguientes equipamientos"),
          class = "infra-frase"
        )
      })

      output$equipamiento_list <- renderUI({
        ids <- equip_inputs()
        if (length(ids) == 0) return(NULL)##Esto nunca debería ocurrir
        choices_available <- equipamiento_opciones()[['catalogo']]
        
        tagList(
          lapply(seq_along(ids), function(i) {##Este es tal cual lo que se hace en update map
            id <- ids[i]
            selected <- equip_defaults()[[id]]
            div(
              class = "infra-input-card",
              selectizeInput(ns(id), label = NULL, choices = choices_available, selected = selected,
                             options = list(create = FALSE, placeholder = 'Escribe o selecciona...', openOnFocus = TRUE, allowEmptyOption = TRUE),
                             width = "100%"),
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
      
      last_selected_values <- reactiveVal(character(0))
      skip_next_observer_update <- reactiveVal(FALSE)
      
      ##Observe nivel_at changes
      observeEvent(c(selected_tab(), nivel_at()), {
        last_selected_values(character(0))
        inputs_initialized(character(0))
        skip_next_observer_update(TRUE)  ## Flag to skip the next observer firing
        update_mapa()
      }, ignoreInit = TRUE)
      
      observe({
        if (skip_next_observer_update()) {
          skip_next_observer_update(FALSE)
          return(invisible(NULL))
        }
        
        ids <- equip_inputs()
        current_values <- unique(na.omit(c(sapply(ids, function(x) {
          val <- input[[x]]
          if (is.null(val) || val == "") NA else val
        }))));
        ##Only update map if the actual selected VALUES changed
        if (!identical(sort(current_values), sort(last_selected_values())) ) {
          last_selected_values(current_values)
          update_mapa()
        }
      }, label = "equipment-values-watcher")
      
    }
  )
}


