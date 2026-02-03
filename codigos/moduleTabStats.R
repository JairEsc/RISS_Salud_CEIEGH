tabStatsUI=function(id){
  ns <- NS(id)
  tabItem(tabName = "stats",
          sliderInput(ns("sliderTiempo"),min = 0,max = 180,value = 60,label = "Tiempo de accesibilidad"),
          fluidRow(
            valueBoxOutput(ns("total_pob"), width = 3),
            valueBoxOutput(ns("total_afiliada"), width = 3),
            valueBoxOutput(ns("total_mun"), width = 3),
            valueBoxOutput(ns("total_loc"), width = 3)
          ),
          fluidRow(
            column(width = 7,
                     box(width = NULL, title = "Visualización Espacial", status = "primary", solidHeader = TRUE,
               leafletOutput(ns("mapa_stats"), height = "60vh")
                     )
            ),
            column(width = 5,
                   div(style = "display:flex; justify-content: flex-end; margin-bottom:8px;",
                     downloadButton(ns("download_current"), label = "Descargar", class = "btn-primary")
                   ),
                   tabBox(width = NULL, title = "Desglose de Datos", id = ns("tab_detalle"),
                          tabPanel("Municipios", DT::DTOutput(ns("tabla_mun"))),
                          tabPanel("Localidades", DT::DTOutput(ns("tabla_loc"))),
                          tabPanel("AGEBs", DT::DTOutput(ns("tabla_ageb")))
                   )
            )
          )
  )
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
    output$tabla_mun  <- DT::renderDT({
      datatable(listas_estadisticas()[[3]], 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = 'cell-border stripe'
                )
    })
    output$tabla_loc  <- DT::renderDT({
      datatable(listas_estadisticas()[[2]], 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = 'cell-border stripe')
    })
    output$tabla_ageb <- DT::renderDT({
      datatable(listas_estadisticas()[[1]] |> st_drop_geometry(), 
                options = list(pageLength = 10, scrollX = TRUE, dom = 'ftp',
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
                rownames = FALSE, class = 'cell-border stripe')
    })
    output$mapa_stats <- renderLeaflet({
      lst <- listas_estadisticas()
      capa_sf <- lst[[1]]
      m <- leaflet() |> 
        addProviderTiles(providers$CartoDB.Positron) |> 
        setView(lng = -102, lat = 23, zoom = 5)
      if(!is.null(capa_sf) && nrow(capa_sf) > 0){
        pal <- colorNumeric(palette = "YlOrRd", domain = c(0, max(c(0, max(capa_sf$POB_rel, na.rm = TRUE)))))
        m <- m |> addPolygons(data = capa_sf,
                              fillColor = pal(capa_sf$POB_rel),
                              color = pal(capa_sf$POB_rel), weight = 5,
                              label = ~paste0("CVEGEO: ", CVEGEO, " (", POB_rel, "%)")
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
                    fillColor =pal(capa_sf$POB_rel),
                    color=pal(capa_sf$POB_rel),weight = 5,
                    label = ~paste0("CVEGEO: ", capa_sf$CVEGEO, " (", capa_sf$POB_rel, "%)")
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

    
