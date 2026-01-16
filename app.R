##A partir de la propuesta de trabajo del CEIEGH: 

###SideBar + Mapa principal + (por definir)

#En el sidebar se puede elegir el nivel de atención de clues en operación

#La elección de estas definen una consulta a clues de tipo punto. 
#Se calcula la accesibilidad (cacheada a través de .tif)

#En el mapa principal se agregan AGEBs. La elección de las clues define una repartición de población de AGEBs entre las clues a accesibilidad digna
###Coneval: tiempos promedio de traslado: https://www.coneval.org.mx/Informes/Evaluacion/Impacto/Acceso%20y%20Uso%20Efectivo.pdf (58 minutos)
###Viene desagregado por tipo de afiliación, btw.

#Propuesta: Un click sobre un clues dibuja la isocrona a niveles fijos. Que en teoría es consistente con la accesibilidad del sigeh
            #pob a distancia digna total
            #pob a distancia digna por afiliacion, etc.
#Propuesta: Un click sobre un AGEB muestra información de clues a menos de 58 minutos, (o 60),
            #pob total
            #pob por afiliacion, etc.

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinydashboard)
library(shinydashboardPlus)

source("codigos/csv_to_geojson.R")
source("../../Reutilizables/Postgres_BUIG/token_mapbox.R")
source("codigos/funciones.R")
source("../../Reutilizables/Postgres_BUIG/conexion_local.R")
source("codigos/SIGEH_isochrone.R")


#DBI::dbCreateTable(conn = local,name = "clues_en_operacion",fields = clues_en_operacion )
#DBI::dbWriteTable(conn = local,name = "clues_en_operacion",value = clues_en_operacion ,binary=T,overwrite=T)
#sf::st_write(clues_en_operacion,local,"clues_en_operacion",delete_layer = TRUE, append = FALSE)
#dplyr::tbl(local,"clues_en_operacion") |> dplyr::collect() |> class()
clues_en_operacion=dplyr::tbl(local,"clues_en_operacion")
# con <- dbConnect(Postgres())
# 
# dbExecute(con, "INSTALL spatial; LOAD spatial;")


paleta_spectral_comun=colorNumeric(palette = "Spectral",domain = c(10,20,40,60))

##En muestra de regionalizacion definimosageb_pob y ageb_geo. Lo ideal sería recuperar lo mínimo para su definicion. 
# ageb_loc=rbind(ageb_geo union ageb_pob,
#                loc urbana sin agebs  + loc_rural_exterior sin agebs+
#                  loc_rural_puntual sin agebs
#                )

#ageb_loc=ageb_pob
#demograficos_scince proviene de definicion_cartografia_demografia


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "RISS",disable = F),
  
  shinydashboardPlus::dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    uiOutput("userpanel"),
    
    div(class = "sidebar-controls",
        selectInput("nivel_at",
                    label = "Nivel de atención",
                    choices = c("1er nivel" = "PRIMER NIVEL",
                                "2do nivel" = "SEGUNDO NIVEL",
                                "3er nivel" = "TERCER NIVEL"),
                    selectize = TRUE,selected ="SEGUNDO NIVEL" )
    )
    ,shinyjs::useShinyjs(),
    
    checkboxInput(inputId = "agebs",label = "AGEBs",value = F),
    
    sidebarMenu(
      menuItem("Mapa Principal", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Estadísticas", tabName = "stats", icon = icon("chart-bar"))
    )
    ,collapsed = F,minified = T
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, class = "map-box",
                    leafletOutput("mapa_principal", width = "100%", height = "80vh")
                )
              )
      )
    )
  )
)

shinyApp(ui, function(input, output) {
  output$mapa_principal=renderLeaflet({
    leaflet() |> addTiles() |> leaflet.extras::addDrawToolbar(targetGroup = "especiales",position = "topleft",
                                                              polylineOptions =F,circleOptions = F,rectangleOptions = F,markerOptions = F,
                                                              circleMarkerOptions = F,editOptions = editToolbarOptions(edit = T,remove = T,allowIntersection = F))
  })
  #Agregar AGEBs
  
  #Agregamos el select (nivel de atencion) con debounce
  input_nivel_at=reactive({
    input$nivel_at
  })
  input_nivel_at_d=input_nivel_at |> debounce(1000)
  observeEvent(input_nivel_at_d(),
    {
      print(input$nivel_at)
      zz<-clues_en_operacion |> dplyr::filter(NIVEL.ATENCION==input$nivel_at) |> dplyr::select(geometry) |> dplyr::collect() |> 
        dplyr::mutate(geometry=st_as_sfc(geometry)) |> st_as_sf()
      showNotification(paste0(nrow(zz)," CLUES de ",stringr::str_to_lower(input$nivel_at)) )
      tiempo_zona=accCost(T.GC, matrix(unlist(zz$geometry |> st_transform(32614)),nrow = nrow(zz),ncol = 2,byrow = T))
      crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
      tiempo_zona[ is.infinite(tiempo_zona)]=300
      tiempo_zona[ tiempo_zona>=90]=NA
      #extent(tiempo_zona)=projectExtent(tiempo_zona,crs = st_crs("EPSG:4326")$wkt)
      #tiempo_zona
      #plot(tiempo_zona)
      iso1_sigeh=raster::rasterToContour(tiempo_zona, levels = c(10,20,40,60))|> st_as_sf() |> st_set_crs(st_crs("EPSG:32614")) |>st_transform(st_crs("EPSG:4326"))
      como_se_veria_iso1_sigeh<<-iso1_sigeh 
      leafletProxy("mapa_principal") |> 
        clearMarkers() |> 
        clearImages() |> 
        removeShape(layerId = paste0("Isocronas",1:4)) |> 
        removeControl(layerId = "Accesibilidad en minutos2") |>
        addMarkers(data=zz,group = "CLUES") |>
        setView(lng = -98.83284,lat = 20.45979,zoom = 9) |> 
        addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = "Spectral",group = "Accesibilidad en minutos") |> 
        addPolylines(data=iso1_sigeh  ,
                     color=paleta_spectral_comun(iso1_sigeh$level |> as.numeric()),opacity = 1,group = "Isocronas",layerId = paste0("Isocronas",1:4)) |>
        addLegend(
          position = "bottomright",
          pal = colorNumeric(palette = "Spectral", domain = c(10, 90)),
          values = c(10, 20, 40, 60,90),
          group = "Accesibilidad en minutos",
          # Aquí forzamos las etiquetas personalizadas:
          labFormat = function(type, cuts, p) {
            return(c("<10 min.", "<20 min.", "<40 min.", "<60 min.","<90 min."))
          }
        ,layerId = "Accesibilidad en minutos2") |> 
        addLayersControl(overlayGroups = c("Accesibilidad en minutos", "Isocronas","CLUES"))
  })
  #Agregamos el checkbox (agebs) con debounce
  input_checkbox_agebs=reactive({
    input$agebs
  })
  input_checkbox_agebs_d=input_checkbox_agebs |> debounce(500)
  observeEvent(input_checkbox_agebs_d(),
    {
      if(input$agebs==T){
        print(input$agebs)
        leafletProxy("mapa_principal") |>
          addPolygons(data=demograficos_scince,label = paste0(demograficos_scince$CVEGEO,"<br>",
                                                   "Pob. Total:  ",demograficos_scince$POB1,"<br>",
                                                   "Pob. Afiliada SS:  ",demograficos_scince$SALUD1,"<br>"
          ) |> lapply(\(x){htmltools::HTML(x)}),
          group="AGEBs",layerId = paste0("AGEBs",1:nrow(demograficos_scince)) )
      }
      else{
        print(input$agebs)
        leafletProxy("mapa_principal") |>
          removeShape(paste0("AGEBs",1:nrow(demograficos_scince)))
      }
      
  })
  lista_objetos_especiales=reactiveVal(value = 0)
  observeEvent(input$mapa_principal_marker_click,{
  #   print(input$mapa_principal_click)
  #   ##Un click sobre el mapa 
  #   ###3 Casos: clcick sobre clues, poligono (poblacion), raster. 
  #   
  #   #Caso clues:
  #   ##Nombre, ubicacion (mun + loc)
  #   ##Construcción de isocrona a niveles fijos.
  
  ###Construimos el punto a partir del click.
    print(input$mapa_principal_marker_click)
    punto_referencia_fijo=st_point(c(input$mapa_principal_marker_click$lng ,input$mapa_principal_marker_click$lat)) |> st_sfc(crs = 4326)
    isocronas_niveles_fijos=getIsochrones_mapbox(coord = punto_referencia_fijo |> unlist() |> paste(collapse = ","),
                       times =c(10,20,40,60) ) |> st_as_sf() |> st_transform(st_crs("EPSG:4326"))
    print(isocronas_niveles_fijos)
    leafletProxy("mapa_principal") |> 
      #clearGroup(group = "especiales") |> 
      addPolygons(data=isocronas_niveles_fijos,group = "especiales",
                  color=paleta_spectral_comun(isocronas_niveles_fijos$contour |> as.numeric()),opacity = 1,fillColor =paleta_spectral_comun(isocronas_niveles_fijos$contour |> as.numeric()),fillOpacity = 0.7 )
    
    ##Cuando se agregue una capa de dibujo se prende el botoncito para borrar. Cuando se limpie todo, se descolorea. 
    ##leaflet-draw-edit-remove
    lista_objetos_especiales(1)
    
    
    # if(length(input$mapa_principal_draw_all_features)==0){
    #   shinyjs::addClass()
    # }
  #   
  #   #Caso poligono
  #   ##Poblaciones, ubicacion, etc. (fijos)
  #   ##Poblaciones por tipo de derechohabiencia
  #   ##Clues cercanos (<58 min según coneval)
  #   
  #   #Caso raster.
  #   ## Probar obtener el tiempo promedio. 
  #   ## coordenada
  #   ## Podría ser una vectorización simple del raster para sacar el promedio. 
  #   
  })
  
  observeEvent(input$mapa_principal_draw_all_features,{
    if(length(input$mapa_principal_draw_all_features==0){
      lista_objetos_especiales(0)
    }
  })
  observe(lista_objetos_especiales,{
    if(lista_objetos_especiales==0){
      shinyjs::runjs("console.log('prueba')")
      shinyjs::runjs(code = "
                   let botonBorrar=document.getElementsByClassName('leaflet-draw-edit-remove')[0]
                   console.log(botonBorrar)
                   botonBorrar.classList.remove('colorRojo')
                   ")
    }
    else{
      shinyjs::runjs("console.log('prueba')")
      shinyjs::runjs(code = "
                   let botonBorrar=document.getElementsByClassName('leaflet-draw-edit-remove')[0]
                   console.log(botonBorrar)
                   botonBorrar.classList.add('colorRojo')
                   ")
    }
  })
  
  
})
