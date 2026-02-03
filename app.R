##A partir de la propuesta de trabajo del CEIEGH: 

###SideBar + Mapa principal + (por definir. Posiblemente graphs)

#En el sidebar se puede elegir el nivel de atención de clues en operación y los agebs (unidades de poblacion)

#La elección de estas definen una consulta a clues de tipo punto. 
#Se calcula la accesibilidad (se puede optimizar el cálculo de accesibilidad si pre-cargamos los puntos)

#En el mapa principal se agregan AGEBs. La elección de las clues define una repartición de población de AGEBs entre las clues a accesibilidad digna
###Coneval: tiempos promedio de traslado: https://www.coneval.org.mx/Informes/Evaluacion/Impacto/Acceso%20y%20Uso%20Efectivo.pdf (58 minutos)
###Viene desagregado por tipo de afiliación, btw.

#Propuesta: Un click sobre un clues dibuja la isocrona a niveles fijos. Que en teoría es consistente con la accesibilidad del sigeh
            #pob a distancia digna total
            #pob a distancia digna por afiliacion, etc.
          #Una sola CLUES tiene cobertura de tanta poblacion a tanto minutos.
#Propuesta: Un click sobre un AGEB muestra información de clues a menos de 58 minutos, (o 60),
            #pob total
            #pob por afiliacion, etc.
          #Conteo del número de clues por rango de tiempo

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(raster)
library(shinydashboard)
library(shinydashboardPlus)

#source("codigos/csv_to_geojson.R")
source("codigos/token_mapbox.R")
source("codigos/funciones.R")
source("../../Reutilizables/Postgres_BUIG/conexion_local.R")#Aislar
##Ya está aislada en supabase. Para leerla de texto a hexadecimal:
#clues_en_operacion |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T))
source("codigos/SIGEH_isochrone.R")
source("codigos/definicion_cartografia_demografia.R")
source("codigos/definicion_custom_markers.R")


clues_en_operacion=dplyr::tbl(local,"clues_en_operacion")
# 
paleta_spectral_comun=colorNumeric(palette = "Spectral",domain = c(10,20,40,60,90))

#demograficos_scince proviene de definicion_cartografia_demografia


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "RISS",disable = F),
  
  shinydashboardPlus::dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(
      tags$style(HTML("
    .leaflet-control-layers, .leaflet-control-legend, .info.legend {
      border: none !important;
      border-radius: 12px !important; 
      box-shadow: 0 4px 15px rgba(0,0,0,0.15) !important; 
      padding: 12px !important;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif !important;
      background: rgba(255, 255, 255, 0.9) !important; 
      backdrop-filter: blur(5px); 
    }
    .legend i {
      border-radius: 50%; 
      width: 15px !important;
      height: 15px !important;
      margin-right: 10px !important;
    }
    .legend-title {
      font-weight: bold;
      font-size: 1.1em;
      margin-bottom: 8px;
      color: #2c3e50;
    }
  "))
    ),
    
    uiOutput("userpanel"),
    
    div(class = "sidebar-controls",
        selectInput("nivel_at",
                    label = "Nivel de atención", 
                    choices = c("1er nivel" = "PRIMER NIVEL",
                                "2do nivel" = "SEGUNDO NIVEL",
                                "3er nivel" = "TERCER NIVEL",
                                "Todos los niveles"='CUALQUIER NIVEL'),##Todos los niveles?
                    selectize = TRUE,selected ="SEGUNDO NIVEL" )
    )
    ,shinyjs::useShinyjs(),
    
    checkboxInput(inputId = "agebs",label = "AGEBs y localidades rurales",value = F),
    
    sidebarMenu(
      menuItem("Mapa Principal", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Estadísticas", tabName = "stats", icon = icon("chart-bar"))
    )
    ,collapsed = F,minified = F
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, class = "map-box",
                    leafletOutput("mapa_principal", width = "100%", height = "80vh")
                )
              )
      ),
      tabItem(tabName = "stats",
              fluidRow(
                box(width = 12, class = "map-box",
                    renderText({"Text"})
                    
                )
              )
      )
    )
  )
)

shinyApp(ui, function(input, output) {
  ###Lista de valores reactivos utilizables
  clues_solicitadosss=reactiveValues(df=NULL)
  output$mapa_principal=renderLeaflet({
    #Mapa con tiles por defecto y barra de herramientas para dibujar polígonos
    leaflet() |> addTiles() |> 
      setView(lng = -98.83284,lat = 20.45979,zoom = 9) |> 
      leaflet.extras::addDrawToolbar(targetGroup = "especiales",
                                     position = "topleft",
                                     polylineOptions =F,circleOptions = F,
                                     rectangleOptions = F,markerOptions = F,
                                     circleMarkerOptions = F,
                                     editOptions = editToolbarOptions(edit = T,remove = T,allowIntersection = F)) 
  })
  #Agregamos el select (nivel de atencion) con debounce
  input_nivel_at=reactive({
    input$nivel_at
  })
  input_nivel_at_d=input_nivel_at |> debounce(1000)
  observeEvent(input_nivel_at_d(),
    {
      clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION==input$nivel_at | input$nivel_at=="CUALQUIER NIVEL" ) |> 
        dplyr::select(CLUES,MUNICIPIO,LOCALIDAD,NOMBRE.DE.LA.UNIDAD,NIVEL.ATENCION,CLUESN2_mas_cercana:num_CLUESN1T10,geometry) |>
        dplyr::collect() |> 
        dplyr::mutate(geometry=st_as_sfc(geometry)) |> st_as_sf()
      clues_solicitadosss$df=clues_solicitados
      showNotification(paste0(nrow(clues_solicitados)," CLUES de ",stringr::str_to_lower(input$nivel_at)) )
      tiempo_zona=switch (input$nivel_at,
                          "PRIMER NIVEL" = raster("inputs/rasters/acces_CLUESN1_max90.tif"),
                          "SEGUNDO NIVEL" = raster("inputs/rasters/acces_CLUESN2_max90.tif"),
                          "TERCER NIVEL" = raster("inputs/rasters/acces_CLUESN3_max90.tif"),
                          "CUALQUIER NIVEL" = raster("inputs/rasters/acces_CLUES_max90.tif")
      )
      iso1_sigeh=raster::rasterToContour(tiempo_zona, levels = c(10,20,40,60,90))|> st_as_sf() |> st_set_crs(st_crs("EPSG:32614")) |>st_transform(st_crs("EPSG:4326"))
      leafletProxy("mapa_principal") |> ##Esta función se puede generalizar y aislar
        clearMarkers() |> 
        clearImages() |> 
        clearGroup("CLUES") |> 
        removeShape(layerId = paste0("Isocronas",1:nrow(iso1_sigeh))) |> 
        removeControl(layerId = "Accesibilidad en minutos2") |>
        addMarkers_custom(data = clues_solicitados) |> 
        addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = "Spectral",group = "Accesibilidad en minutos") |> 
        addPolylines(data=iso1_sigeh  ,
                     color=paleta_spectral_comun(iso1_sigeh$level |> as.numeric()),opacity = 1,group = "Isocronas",layerId = paste0("Isocronas",1:nrow(iso1_sigeh))) |>
        addLayersControl(overlayGroups = c("Accesibilidad en minutos", "Isocronas","CLUES"))
  })
  
  #Agregamos el checkbox (agebs) con debounce
  input_checkbox_agebs=reactive({
    input$agebs
  })
  input_checkbox_agebs_d=input_checkbox_agebs |> debounce(500)
  observeEvent(input_checkbox_agebs_d(),##Esta función se puede aislar
    {
      if(input$agebs==T){
        leafletProxy("mapa_principal") |>
          addPolygons(data=demograficos_scince,label = paste0(demograficos_scince$CVEGEO,"<br>",
                                                   "Pob. Total:  ",demograficos_scince$POB1,"<br>",
                                                   "Pob. Afiliada SS:  ",demograficos_scince$SALUD1,"<br>"
          ) |> lapply(\(x){htmltools::HTML(x)}),
          group="AGEBs",layerId = paste0("AGEBs",1:nrow(demograficos_scince)))
      }
      else{
        print(input$agebs)
        leafletProxy("mapa_principal") |>
          removeShape(paste0("AGEBs",1:nrow(demograficos_scince)))
      }
      
  })
  
  
  lista_objetos_especiales <- reactiveVal(value = 0)##Especiales son los que se dibujan. No necesito la lista, nomás saber si está vacía
  
  observeEvent(input$mapa_principal_marker_click,{#2
    datos_del_clues=clues_solicitadosss$df |> 
      dplyr::filter(dplyr::row_number() == as.numeric(gsub("CLUES","",input$mapa_principal_marker_click$id) )) 
    punto_referencia_fijo=st_point(c(input$mapa_principal_marker_click$lng ,input$mapa_principal_marker_click$lat)) |> st_sfc(crs = 4326)
    #isocronas_niveles_fijos=getIsochrones_mapbox(coord = punto_referencia_fijo |> unlist() |> paste(collapse = ","),
    #                                             times =c(10,20,40,60) ) |> st_as_sf() |> st_transform(st_crs("EPSG:4326"))
    isocronas_niveles_fijos <- tryCatch({
      res_raster <- accCost(T.GC, punto_referencia_fijo |> st_transform(st_crs("EPSG:32614")) |> unlist())
      
    contornos <- raster::rasterToContour(res_raster, levels = 10 * c(1:9)) |> 
      st_as_sf() |> 
      st_set_crs(st_crs("EPSG:32614"))
      
    contornos 
    }, error = function(e) {
      message("Error en accCost: Generando círculos concéntricos como respaldo.")
      punto_proyectado <- punto_referencia_fijo |> st_transform(st_crs("EPSG:32614"))
      # Creamos una secuencia de radios
      radios <- seq(100, 2500, by = 300)
      circulos <- do.call(rbind, lapply(radios, function(r) {
        st_buffer(punto_proyectado, dist = r) |> st_as_sf() |> 
          dplyr::mutate(level = as.character(r / 30))
      }))
      
      return(circulos)
    })
    isocronas_niveles_fijos <- isocronas_niveles_fijos |> 
      dplyr::arrange(dplyr::desc(level)) |> 
      st_transform(st_crs("EPSG:4326"))
    
    leafletProxy("mapa_principal") |> 
      addPolygons(
        data = isocronas_niveles_fijos,
        group = "especiales",
        color = paleta_spectral_comun(as.numeric(isocronas_niveles_fijos$level)),
        opacity = 1,
        fillColor = paleta_spectral_comun(as.numeric(isocronas_niveles_fijos$level)),
        fillOpacity = 0.7
      )
    #Generar polígono y mandar a llamar AccesibilidadPoligono o una variante. 
    
    ##Cuando se agregue una capa de dibujo se prende el botoncito para borrar. Cuando se limpie todo, se descolorea. 
    ##leaflet-draw-edit-remove
    lista_objetos_especiales(1)
    #print(isocronas_niveles_fijos[1,] |> cbind(datos_del_clues |> st_drop_geometry()))
    AccesibilidadCLUES(poligono = isocronas_niveles_fijos[1,] |> cbind(datos_del_clues |> st_drop_geometry()),centro=punto_referencia_fijo)
    ##Al final no estuvo tan chido el mapbox. Da más problemas que soluciones. 
    ##Mejor calcularlo con accesibilidad sigeh y agregar datos de cobertura (pendiente)
  })
  
  observeEvent(input$mapa_principal_draw_all_features,{
    if(length(input$mapa_principal_draw_all_features$features) == 0){
      lista_objetos_especiales(0)
    } else {
      lista_objetos_especiales(1)
    }
  })
  
  observe({
    if(lista_objetos_especiales() == 0){
      shinyjs::runjs(code = "
                   let botonBorrar=document.getElementsByClassName('leaflet-draw-edit-remove')[0]
                   if(botonBorrar){
                     console.log(botonBorrar)
                     botonBorrar.classList.remove('colorRojo')
                   }
                   ")
    }
    else{
      shinyjs::runjs(code = "
                   let botonBorrar=document.getElementsByClassName('leaflet-draw-edit-remove')[0]
                   if(botonBorrar){
                     console.log(botonBorrar)
                     botonBorrar.classList.add('colorRojo')
                   }
                   ")
    }
  })
  observe({
    if ("CLUES" %in% input$mapa_principal_groups & input$nivel_at=='CUALQUIER NIVEL') {
      print("Sí se muestra el legend de clues")
      leafletProxy("mapa_principal") |> 
            addLegend(
              position = "bottomleft",
              colors = unname(colores_markers),
              labels = c("Primer Nivel", "Segundo Nivel", "Tercer Nivel"),
              opacity = 1,
              title = HTML("<div class='legend-title'>Nivel de Atención</div>"),
              group = "CLUES",
              layerId = "leyenda_clues"
            )
    } else {
      leafletProxy("mapa_principal") |> removeControl("leyenda_clues")
    }
  })
  observe({
    if("Accesibilidad en minutos" %in% input$mapa_principal_groups 
       | 
       "Isocronas" %in% input$mapa_principal_groups){
      leafletProxy("mapa_principal") |> addLegend(
        position = "bottomright",
        pal = colorNumeric(palette = "Spectral", domain = c(10, 90)),
        values = c(10, 20, 40, 60, 90),
        title = "Accesibilidad",
        opacity = 0.85,
        #group = "Accesibilidad en minutos",
        layerId = "Accesibilidad en minutos2",
        labFormat = labelFormat(
          suffix = " min.",
          between = " a ",
          transform = function(x) x
        )
      ) 
    }
    else{
      leafletProxy("mapa_principal") |> removeControl("Accesibilidad en minutos2")
    }
  })
  #   #Caso poligono
  #   ##Poblaciones, ubicacion, etc. (fijos)
  #   ##Poblaciones por tipo de derechohabiencia
  #   ##Clues cercanos (<58 min según coneval)
  #   
  observeEvent(input$mapa_principal_shape_click,{
    ###Solamente si es click sobre un ageb. 
    print(input$mapa_principal_shape_click)
    if(!is.null(input$mapa_principal_shape_click$id)){
      print("id-------------")
      print(input$mapa_principal_shape_click$id)
      if(grepl(pattern = "AGEB",x = input$mapa_principal_shape_click$id) ){
        
        poligono=demograficos_scince[as.numeric(gsub("AGEBs","",input$mapa_principal_shape_click$id)),]
        AccesibilidadPoligono(poligono)

      }
    }

  })
  observeEvent(input$mapa_principal_draw_new_feature,{
    cat("\n\nNew Feature\n")
    data <- input$mapa_principal_draw_new_feature # list
    data <- jsonlite::toJSON(data, auto_unbox = TRUE) # string
    data <- geojsonio::geojson_sf(data) # sf
    ##Dado un dibujo, se calculan las intersecciones no vacías, se estima la población y viviendas
    ##Se estima la accesibilidad a CLUES por rangos
    interseccion_agebs=st_intersection(demograficos_scince,y = data)
    n_poligonos_involucrados=interseccion_agebs |> nrow()
    ##Resumir las intersecciones como la suma.
    data_c_geo=data |> dplyr::bind_cols( interseccion_agebs |> 
                               dplyr::select(POB1:SALUD10,CVEGEO,NOM_MUN:NOMGEO,tiempo_promedio_CLUES_N1:tiempo_promedio_CLUES_N2,CLUES:NOMBRE.DE.LA.UNIDAD) |>  
                                 st_drop_geometry() |> 
                               dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(.x < 0, NA, .x)
                                                           )) |> 
                                 dplyr::summarise_all(.funs = \(x){ifelse(is.character(x),paste0(unique(x),collapse = ", "),sum(x,na.rm=T))}) ) |> 
      dplyr::mutate(
        tiempo_promedio_CLUES_N1=tiempo_promedio_CLUES_N1/n_poligonos_involucrados,
        tiempo_promedio_CLUES_N2=tiempo_promedio_CLUES_N2/n_poligonos_involucrados,
                    )
      
    ##Se reutiliza el método de arriba con este polígono nuevo.
    AccesibilidadPoligono(data_c_geo)
  })
})

#shiny::runApp("app.R",host = "0.0.0.0", port = 80)