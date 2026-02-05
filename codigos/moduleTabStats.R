tabStatsUI=function(id){
  ns <- NS(id)
  tabItem(tabName = "stats",
          introBox(id = "tour_step_4_slider",data.step = 4,data.intro = "<b>Filtrar por Tiempo de Accesibilidad</b><br/>Usa este deslizador para seleccionar un tiempo en minutos. El sistema filtrará las localidades que quedan fuera de ese rango de tiempo, es decir, con un CLUES más lejos que el tiempo seleccionado.",
            tags$style(HTML(sliderInputTiempoCss)),
            div(class = "slider-filter-container",
              sliderInput(ns("sliderTiempo"),width = '60%',min = 0,max = 180,value = 58,
                          label = "Seleccionar tiempo mínimo requerido (en minutos) para llegar al CLUES más cercano"),
                div(class = "slider-filter-note",
                span(class = "filter-note-icon", icon("exclamation-triangle")),
                span("Filtra localidades donde el tiempo promedio a un CLUES es ", tags$b("mayor"), " al tiempo seleccionado")
              ))
            ,
          fluidRow(
            valueBoxOutput(ns("total_pob"), width = 3),
            valueBoxOutput(ns("total_afiliada"), width = 3),
            valueBoxOutput(ns("total_mun"), width = 3),
            valueBoxOutput(ns("total_loc"), width = 3)
          ),
          fluidRow(
            column(width = 7,
                     box(width = NULL, title = "AGEBs y localidades rurales con tiempo promedio de llegada al CLUES más cercano mayor al tiempo seleccionado", status = "primary", solidHeader = TRUE,
               leafletOutput(ns("mapa_stats"), height = "55vh")
                     )
            ),
            column(width = 5,
                   introBox(id = "tour_step_5_download",data.step = 5,data.intro = "<b>Descargar Datos</b><br/>Descarga los datos filtrados por tiempo en diferentes formatos (XLSX para municipios/localidades, GeoJSON para AGEBs).",
                     div(style = "display:flex; justify-content: flex-end; margin-bottom:8px;",
                       downloadButton(ns("download_current"), label = "Descargar", class = "btn-primary")
                     )
                   ),
                   introBox(id = "tour_step_6_table",data.step = 6,data.intro = "<b>Tabla de Desglose</b><br/>Visualiza los datos detallados por municipios, localidades o AGEBs. Los datos se actualizan automáticamente según el tiempo seleccionado.",
                     tabBox(height = "55vh",width = NULL, title = "Agrupamiento de datos", id = ns("tab_detalle"),
                            tabPanel("Municipios", DT::DTOutput(ns("tabla_mun"))),
                            tabPanel("Localidades", DT::DTOutput(ns("tabla_loc"))),
                            tabPanel("AGEBs", DT::DTOutput(ns("tabla_ageb")))
                     )
                   )
            )
          )
  ))
}

tabStatsServer <- function(id, nivel_at) {
  moduleServer(
    id,
    function(input, output, session) {
    listas_estadisticas <- reactive({
      req(input$sliderTiempo, nivel_at()) 

      estadisticas_dado_nivel_atencion_y_tiempo(
        nivel = nivel_at(), 
        tiempo = input$sliderTiempo
      )
    })
    
    #Renderizar cajitas de valores
    output$total_pob <- renderValueBox({
      res <- listas_estadisticas()[[3]]
      valueBox(
        format(sum(res$POB1, na.rm = TRUE), big.mark = ","),
        "Población Total", icon = icon("users"), color = "teal"
      )
    })
    
    output$total_afiliada <- renderValueBox({
      res <- listas_estadisticas()[[3]]
      valueBox(
        format(sum(res$SALUD1, na.rm = TRUE), big.mark = ","),
        "Población Afiliada", icon = icon("hospital-user"), color = "teal"
      )
    })
    
    output$total_mun <- renderValueBox({
      res <- listas_estadisticas()[[3]]
      valueBox(nrow(res), "Municipios fuera de cobertura", icon = icon("map"), color = "teal")
    })
    
    output$total_loc <- renderValueBox({
      res <- listas_estadisticas()[[2]]
      valueBox(nrow(res), "Localidades", icon = icon("house-circle-exclamation"), color = "teal")
    })
    output$tabla_mun <- DT::renderDT({
      datatable(listas_estadisticas()[[3]] |> st_drop_geometry(), 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = 'cell-border stripe',
                colnames = c('Municipio'='NOM_MUN','Población Total'='POB1',
                             'Población Afiliada a SS'='SALUD1',
                             'Tiempo promedio a CLUES cualquier nivel'='tiempo_promedio_CLUES','Tiempo promedio a CLUES N1'='tiempo_promedio_CLUES_N1',
                             'Tiempo promedio a CLUES N2'='tiempo_promedio_CLUES_N2',
                             'Tiempo promedio a CLUES N3'='tiempo_promedio_CLUES_N3'))
    })
    output$tabla_loc  <- DT::renderDT({
      datatable(listas_estadisticas()[[2]], 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = 'cell-border stripe',
                colnames = c('Municipio'='NOM_MUN','Localidad'='NOMGEO','Población Total'='POB1',
                             'Población Afiliada a SS'='SALUD1',
                             'Tiempo promedio a CLUES cualquier nivel'='tiempo_promedio_CLUES','Tiempo promedio a CLUES N1'='tiempo_promedio_CLUES_N1',
                             'Tiempo promedio a CLUES N2'='tiempo_promedio_CLUES_N2',
                             'Tiempo promedio a CLUES N3'='tiempo_promedio_CLUES_N3'))
    })
    output$tabla_ageb <- DT::renderDT({
      datatable(listas_estadisticas()[[1]] |> st_drop_geometry(), 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = 'cell-border stripe',
                colnames = c('Municipio'='NOM_MUN','Localidad'='NOMGEO','Población Total'='POB1','Población Hombres'='POB42',
                             'Población Mujeres'='POB84',
                             'Población Afiliada a SS'='SALUD1','Tiempo promedio a CLUES N1'='tiempo_promedio_CLUES_N1',
                             'Tiempo promedio a CLUES N2'='tiempo_promedio_CLUES_N2',
                             'Tiempo promedio a CLUES N3'='tiempo_promedio_CLUES_N3',
                             'Tiempo promedio a CLUES de cualquier nivel'='tiempo_promedio_CLUES',
                             'CLUES N2 más cercano'='CLUES','Nombre del CLUES más cercano'='NOMBRE.DE.LA.UNIDAD',
                             'Núm.  de CLUES N1 a menos de 10 mins.'='CLUES_N1_10',
                             'Núm.  de CLUES N1 a menos de 60 mins.'='CLUES_N1_60','Porcentaje %'='POB_rel')
                )
    })
    output$mapa_stats <- renderLeaflet({
      lst <- listas_estadisticas()
      capa_sf <- lst[[1]]
      m <- leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -98.83284,lat = 20.45979,zoom = 8) 
      if(!is.null(capa_sf) && nrow(capa_sf) > 0){
        pal <- colorNumeric(palette = "YlOrRd", domain = c(0, max(c(0, max(capa_sf$POB_rel, na.rm = TRUE)))))
        m <- m |> addPolygons(data = capa_sf,
                              fillColor = pal(capa_sf$POB_rel),fillOpacity = 0.9,
                              color = pal(capa_sf$POB_rel), weight = 7,opacity = 1,
                              label = ~paste0("CVEGEO: ", CVEGEO, " (", POB_rel, "%)"),
                              popup = lst[[4]]
        )
      }
      m
    })
    
    observe({
      capa_sf <- listas_estadisticas()[[1]]
      pal <- colorNumeric(palette = "YlOrRd", domain = c(0, 
                                                         max(c(0,max(listas_estadisticas()[[1]]$POB_rel))
                                                         )))##El máximo de un conjunto vacío es -Inf, por eso tomamos el máximo

      leafletProxy("mapa_stats", session = session) |>
        clearShapes() |>
        addPolygons(data = capa_sf,
                    fillColor =pal(capa_sf$POB_rel),fillOpacity = 0.9,
                    color=pal(capa_sf$POB_rel),weight = 7,opacity = 1,
                    label = ~paste0("CVEGEO: ", capa_sf$CVEGEO, " (", capa_sf$POB_rel, "%)"),
                    popup = listas_estadisticas()[[4]]
        )
      #Agregar clues?
    })
    output$download_current <- downloadHandler(
      filename = function() {
        tab <- input$tab_detalle
        tab_clean <- gsub("\\s+", "_", tolower(tab))
        ext <- if(tab %in% c("Municipios", "Localidades")) ".xlsx" else ".geojson"
        paste0(tab_clean, "_", Sys.Date(), ext)
      },
      content = function(file) {
        tab <- input$tab_detalle
        if(tab == "Municipios"){
          mun <- listas_estadisticas()[[3]]
          mun_df <- tryCatch({ sf::st_drop_geometry(mun) }, error = function(e) mun)
          
          openxlsx::write.xlsx(mun_df, file)
          
        } else if(tab == "Localidades"){
          loc <- listas_estadisticas()[[2]]
          loc_df <- tryCatch({ sf::st_drop_geometry(loc) }, error = function(e) loc)
          writexl::write_xlsx(loc_df, path = file)
         
        } else if(tab == "AGEBs"){
          ageb_sf <- listas_estadisticas()[[1]]
          # write GeoJSON directly to 'file'
          sf::st_write(ageb_sf, file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
        } else {
          # fallback: write a small text file
          writeLines("No data for selected tab.", con = file)
        }
      }
    )
    return(listas_estadisticas)
    }
  )
}

    
