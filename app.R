##A partir de la propuesta de trabajo del CEIEGH: 

###SideBar + Mapa principal(Accesibilidad) + Estadísticas (Cobertura) + Infraestructura (SINERHIAS) 

#En el sidebar se puede elegir el nivel de atención de clues en operación Y privado/publico, 
#- Los agebs (unidades de poblacion)

#La elección de estas definen una consulta a clues de tipo punto. 
  ## Nivel_K públicos y/o Nivel_K privados

#Se calcula la accesibilidad para la elección generada. (Precargados a partir de rasters)

#En el mapa principal se agregan AGEBs. 

###Coneval: tiempos promedio de traslado: https://www.coneval.org.mx/Informes/Evaluacion/Impacto/Acceso%20y%20Uso%20Efectivo.pdf (58 minutos es el indicador de tiempo estimado de traslado en caso de presentarse una emergencia Fuente: Elaboración del CONEVAL con datos del MCS-ENIGH 2008 y 2010.)
###Viene desagregado por tipo de afiliación, btw.

#Accesibilidad: 
###-Un click sobre un clues dibuja la isocrona a niveles fijos. Que en teoría es consistente con la accesibilidad del sigeh
            #-Poblacion estimada a 10 y 60 minutos
            #-Poblacion estimada afiliada a SS a 10 y 60 minutos
            #-Número de clues de nivel 2 (hospitales) a menos de 10 minutos
###-Un click sobre un AGEB muestra información
            #pob total
            #municipio y localidad
            #pob afiliada a ss
            #Tiempo promedio a CLUES N1 y Nivel 2 más cercano
### Se pueden seleccionar varios AGEBS y genera un resumen por sumas y promedios.
#Cobertura:
###-Filtro sobre tiempo: Elegir un número equivale a filtrar localidades que tienen CLUES a más de tantos minutos
###-Tarjetas de resumen con sumas: Pob Total, Pob Afiliada a SS, Municipios y localidades sin cobertura 
###-Mapa mostrando las localidades fuera de cobertura
###-Opción para descargar Municipios, Localidad y AGEBS. Información por grupos etarios agregada.
#Infraestructura:
##Solamente hay datos para tipo público. 
##Dado un catálogo, permite filtrar CLUES del nivel seleccionado que satisfacen la condición de intersección (Y) de las condiciones agregadas

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

source("codigos/SIGEH_isochrone.R")
source("codigos/definicion_cartografia_demografia.R")#demograficos_scince
source("codigos/definicion_custom_markers.R")
source("codigos/extras_css.R")
local=DBI::dbConnect(RSQLite::SQLite(), "clues_demograficos_municipios_simple.sqlite")#Contiene CLUES, Municipios y AGEBS
clues_en_operacion=dplyr::tbl(local,"clues_en_operacion")
limites_municipales=sf::st_read(local,"limite_municipal")
lista_rasters=list.files("inputs/rasters/",full.names = T) |> lapply(raster::raster)
##Ya está aislada en supabase. Para leerla de texto a hexadecimal:
#clues_en_operacion |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T))
#Usar .zip 
#temp_dir=tempdir()
#archive::archive_extract(archive = "outputs/confidenciales/clues_SINERHIAS_int.zip",password = Sys.getenv("pass"),dir = temp_dir)
#sinerhias=DBI::dbConnect(RSQLite::SQLite(), list.files(temp_dir,pattern = "clues_SINERHIAS_int.sqlite",full.names = T))

#Usar archivo directo
sinerhias=DBI::dbConnect(RSQLite::SQLite(),  "outputs/confidenciales/clues_SINERHIAS_int.sqlite")

source("codigos/funciones.R")
#Cobertura
source("codigos/moduleTabStats.R")
#Infraestructura
source("codigos/moduleTabInfraestructura.R")



ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Visualizador de la accesibilidad y cobertura de la infraestructura de salud a nivel estatal",disable = F),
  
  shinydashboardPlus::dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(
      tags$style(HTML(leaflet_legend_css))
    ),
    tags$head(
      tags$style(HTML(sidebar_last_child_css)),
      tags$style(HTML("
        .sweet-alert h2 { font-size: 20px !important; margin: 10px 0 !important; }
        .sweet-alert { width: 350px !important; padding: 15px !important; left: 50% !important; margin-left: -175px !important; }
        .sweet-alert .lead { font-size: 14px !important; }
        .shinyalert-checkboxes { text-align: left; margin: 10px auto; width: fit-content; }
      "))
    ),
    uiOutput("userpanel"),
    
    div(class = "sidebar-controls",
        introBox(id = "tour_step_2_nivel", data.step = 1, data.intro = "placeholder",
          div(style='display:flex',
          selectInput("nivel_at",
                      label = "Nivel de atención", 
                      choices = c("1er nivel" = "PRIMER NIVEL",
                                  "2do nivel" = "SEGUNDO NIVEL",
                                  "3er nivel" = "TERCER NIVEL",
                                  "Todos los niveles"='CUALQUIER NIVEL'),
                      selectize = TRUE,selected ="SEGUNDO NIVEL" ),
          actionButton("filtrarPublicoPrivado",class="btn-primary",icon = icon("filter"),label = "", disabled = TRUE)
        )
        )
    ),
    shinyjs::useShinyjs(),
    
    tags$style(HTML(tour_button_css)),
    sidebarMenu(id='sidebarID',
      menuItem("Accesibilidad", tabName = "map", icon = icon("map-marked-alt")),
      introBox(id = "tour_step_3_agebs", data.step = 2, data.intro = "placeholder",
               checkboxInput(inputId = "agebs",label = "AGEBs y localidades rurales",value = F)
      ),
      menuItem("Cobertura", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Infraestructura", tabName = "infra", icon = icon("building")),
      div(style = "padding: 10px;",
          actionButton("start_tour", "Explicación", class="btn-primary", width="100%", icon = icon("question-circle"))
      )
      
    ),
    collapsed = F,minified = F
  ),
  
  dashboardBody(
    introjsUI(),
    tabItems(
      tabItem(tabName = "map",
          fluidRow(
            div(style = "display: flex; justify-content: center; align-items: center; gap: 20px; padding: 0px; height: 8vh;",
          img(src = "images/Logotipo1.png", style = "max-height: 100%; max-width: 35%; object-fit: contain;"),
          img(src = "images/Logotipo armas7.png", style = "max-height: 100%; max-width: 55%; object-fit: contain;")
            )
          ),
          fluidRow(
            introBox(id = "tour_step_1_map", data.step = 3, data.intro = "placeholder",
          box(id='mapa_principal_container',width = 12, class = "map-box",
            leafletOutput("mapa_principal", width = "100%", height = "75vh"),
            add_busy_spinner(spin = "cube-grid")
          )
            )
          )
      ),
      tabStatsUI("tab_stats"),
      tabInfraUI("tab_infra")
    )
  )
)

shinyApp(ui, function(input, output,session) {
  ###Lista de valores reactivos utilizables
  selected_tab <- reactive(input$sidebarID)
  clues_solicitadosss=reactiveValues(df=NULL)
  memoriaPublicosPrivados=reactiveValues(
    nivel_at="SEGUNDO NIVEL",
    publicos = TRUE, 
    privados = TRUE,
    modal_open = FALSE,
    actualizar=FALSE
  )
  output$mapa_principal=renderLeaflet({
    #Mapa con tiles por defecto y barra de herramientas para dibujar polígonos
    ProxyMapaPrincipal(limites_municipales = limites_municipales |> st_transform(4326))
  })
  #Agregamos el select (nivel de atencion) con debounce
  nivel_atencion_gatekeeper_inputs <- reactive({
    if (memoriaPublicosPrivados$modal_open) {
      req(FALSE) # Cortar proceso
    }
    list(
      nivel_at = memoriaPublicosPrivados$nivel_at,
      publicos = memoriaPublicosPrivados$publicos,
      privados = memoriaPublicosPrivados$privados,
      actualizar=memoriaPublicosPrivados$actualizar
    )
  })
  
  input_nivel_at_d <- nivel_atencion_gatekeeper_inputs |> debounce(1000)

  observeEvent(c(selected_tab(), input_nivel_at_d()),
    {
      req(selected_tab() == "map")
      req(memoriaPublicosPrivados$actualizar)
      tipo_filtro <- c()
      if(input_nivel_at_d()$publicos) tipo_filtro <- c(tipo_filtro, "Público")
      if(input_nivel_at_d()$privados) tipo_filtro <- c(tipo_filtro, "Privado")
      clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION==input$nivel_at | input$nivel_at=="CUALQUIER NIVEL" ) |> 
        dplyr::filter(archivo_origen%in%tipo_filtro) |> 
        dplyr::select(CLUES,MUNICIPIO,LOCALIDAD,NIVEL.ATENCION,Conteo_N1_T10:SALUD10_T60,geometry) |>
        dplyr::collect() |> 
        dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
      clues_solicitadosss$df=clues_solicitados
      elegirRaster=function(nivel_at,tipo_filtro){
        if(length(tipo_filtro)==2){
          query=""
        }else{
          query=paste0(tipo_filtro,collapse = "")
        }
        query=paste0("inputs/rasters/acces_",gsub("CUALQUIER_NIVEL","",gsub(pattern = " ",replacement = "_",nivel_at)),
                     "_",gsub(pattern = "ú","u",x = query),".tif" )
        return(query)
      }
      # print("Raster a elegir:")
      # print(elegirRaster(input$nivel_at,tipo_filtro))
      print(paste0(nrow(clues_solicitados)," CLUES de ",stringr::str_to_lower(input$nivel_at)) )
      if(nrow(clues_solicitados)==0){
        return(leafletProxy("mapa_principal") |> ##Esta función se puede generalizar y aislar
               clearProxy(markers = T,images = T,group = "CLUES",shapes =paste0("Isocronas",1:5),controls = "Accesibilidad en minutos2" ) 
        )
      }
      showNotification(paste0(nrow(clues_solicitados)," CLUES de ",stringr::str_to_lower(input$nivel_at)) )
      tiempo_zona_auto=elegirRaster(input$nivel_at,tipo_filtro) |> raster::raster()
      tiempo_zona_peatonal="inputs/rasters/acces_CLUES_max90.tif" |> raster::raster()
      iso1_sigeh=raster::rasterToContour(tiempo_zona_auto, levels = c(10,20,40,60,90))|> st_as_sf() |> st_set_crs(st_crs("EPSG:32614")) |>st_transform(st_crs("EPSG:4326"))
      memoriaPublicosPrivados$actualizar=FALSE
      leafletProxy("mapa_principal") |> ##Esta función se puede generalizar y aislar
        clearProxy(markers = T,images = T,group = "CLUES",shapes =paste0("Isocronas",1:nrow(iso1_sigeh)),controls = "Accesibilidad en minutos2" ) |> 
        addMarkers_custom(data = clues_solicitados) |> 
        addRasterImage(projectRasterForLeaflet(tiempo_zona_auto,method = "ngb"),colors = "Spectral",group = "Accesibilidad carretera (en minutos)") |> 
        addRasterImage(projectRasterForLeaflet(tiempo_zona_peatonal,method = "ngb"),colors = "Spectral",group = "Accesibilidad peatonal (en minutos)") |> 
        addLayersControl(overlayGroups = c("Accesibilidad carretera (en minutos)","Accesibilidad peatonal (en minutos)","CLUES")) |> 
        hideGroup("Accesibilidad peatonal (en minutos)")
  })
  
  #Agregamos el checkbox (agebs) con debounce
  input_checkbox_agebs=reactive({
    input$agebs
  })
  input_checkbox_agebs_d=input_checkbox_agebs |> debounce(100)
  observeEvent(c(selected_tab(), input_checkbox_agebs_d()),##Esta función se aisló
    {
      req(selected_tab() == "map")
      print(input$sidebarID)
      if(input$agebs){
        leafletProxy("mapa_principal") |>
          addPolygons(data=demograficos_scince,label = paste0(demograficos_scince$CVEGEO,"<br>",
                                                              "Pob. Total:  ",demograficos_scince$POB1,"<br>",
                                                              "Pob. Afiliada SS:  ",demograficos_scince$SALUD1,"<br>"
          ) |> lapply(\(x){htmltools::HTML(x)}),
          group="AGEBs",layerId = paste0("AGEBs",1:nrow(demograficos_scince)))
      }
      else{
        leafletProxy("mapa_principal") |>
          removeShape(paste0("AGEBs",1:nrow(demograficos_scince)))
      }
  })

  lista_objetos_especiales <- reactiveVal(value = 0)##Especiales son los que se dibujan. No necesito la lista, nomás saber si está vacía
  
  observeEvent(input$mapa_principal_marker_click,{# Click sobre un clues
    req(selected_tab() == "map")
    #print(clues_solicitadosss$df)
    #print(input$mapa_principal_marker_click)
    datos_del_clues=clues_solicitadosss$df |> ##Estamos conservando todas las columnas del CLUEs aunque no todas se muestran
      dplyr::filter(dplyr::row_number() == as.numeric(gsub("CLUES","",input$mapa_principal_marker_click$id) )) ##Datos del clues seleccionado
    punto_referencia_fijo=st_point(c(input$mapa_principal_marker_click$lng ,input$mapa_principal_marker_click$lat)) |> st_sfc(crs = 4326)
    #print(punto_referencia_fijo)
    isocronas_niveles_fijos <- tryCatch({
      res_raster <- gdistance::accCost(T.GC, punto_referencia_fijo |> st_transform(st_crs("EPSG:32614")) |> unlist())
      
      contornos <- raster::rasterToContour(res_raster, levels = 10 * c(1:9)) |> 
        st_as_sf() |> 
        st_set_crs(st_crs("EPSG:32614"))
      contornos 
    }, error = function(e) {
      message("Error en accCost: Generando círculos concéntricos como respaldo.")
      punto_proyectado = punto_referencia_fijo |> st_transform(st_crs("EPSG:32614"))
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
    ##Lo agregamos al mapa principal
    leafletProxy("mapa_principal") |> 
      addPolygons(
        data = isocronas_niveles_fijos,
        group = "especiales",
        color = paleta_spectral_comun(as.numeric(isocronas_niveles_fijos$level)),
        opacity = 1,
        fillColor = paleta_spectral_comun(as.numeric(isocronas_niveles_fijos$level)),
        fillOpacity = 0.7
      )
    ##Cuando se agregue una capa de dibujo se prende el botoncito para borrar. Cuando se limpie todo, se descolorea. 
    lista_objetos_especiales(1)
    AccesibilidadCLUES(poligono =isocronas_niveles_fijos[1,] |> cbind(datos_del_clues |> st_drop_geometry()) ,centro=punto_referencia_fijo)##Agrega el pop-up con datos demograficos
  })
  
  observe({##Agregar el legend cuando estemos viendo "Cualquier nivel'. Por eso usamos 
    #Observe y no observeEvent. Porque podría haber más de un trigger
    if ("CLUES" %in% input$mapa_principal_groups & input$nivel_at=='CUALQUIER NIVEL') {
      #print("Sí se muestra el legend de clues")
      leafletProxy("mapa_principal") |> addLegend_custom(legendCustom='clues')
    } else {
      leafletProxy("mapa_principal") |> removeControl("leyenda_clues")
    }
  })
  observe({##Agregar el legend cuando estemos viendo un raster de accesibilidad. Ya sea carretera o peatonal
    if("Accesibilidad carretera (en minutos)" %in% input$mapa_principal_groups 
       | 
       "Accesibilidad peatonal (en minutos)" %in% input$mapa_principal_groups){
      leafletProxy("mapa_principal") |> addLegend_custom(legendCustom = "raster")
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
    req(selected_tab() == "map")
    ###Solamente si es click sobre un ageb. 
    if(!is.null(input$mapa_principal_shape_click$id)){
      if(grepl(pattern = "AGEB",x = input$mapa_principal_shape_click$id) ){
        poligono=demograficos_scince[as.numeric(gsub("AGEBs","",input$mapa_principal_shape_click$id)),]
        AccesibilidadPoligono(poligono)##Se mandan todas las columnas aunque no se muestran todas
      }
    }

  })
  observeEvent(input$mapa_principal_draw_new_feature,{
    req(selected_tab() == "map")
    cat("\n\nNew Feature\n")
    data=drawToSf(input$mapa_principal_draw_new_feature)
    #sf
    ##Dado un dibujo, se calculan las intersecciones no vacías, se estima la población y viviendas
    ##Se estima la accesibilidad a CLUES por rangos
    interseccion_agebs=st_filter(demograficos_scince, data)#Se prefiere st_filter sobre st_intersection. Ver no_usar_benchmark_interseccion_poligonos.R
    n_poligonos_involucrados=interseccion_agebs |> nrow()
    
    ##Resumir las intersecciones como la suma
    data_c_geo=data |> dplyr::bind_cols( interseccion_agebs |>
                                           dplyr::select(POB1:SALUD10,CVEGEO,NOM_MUN:NOMGEO,CLUES_N1_10:nombre_clues_N3_mas_cercano) |> ##Aquí sí se eligen menos coluumnas 
                                           st_drop_geometry() |> 
                                           dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(.x < 0, NA, .x) )) |> 
                                           dplyr::summarise_all(.funs = \(x){ifelse(is.character(x),paste0(unique(x),collapse = ", "),sum(x,na.rm=T))}) ) |> ##Suma o concat según el tipo de la variable
      dplyr::mutate(
        tiempo_promedio_clues_N1_mas_cercano=tiempo_promedio_clues_N1_mas_cercano/n_poligonos_involucrados,
        tiempo_promedio_clues_N2_mas_cercano=tiempo_promedio_clues_N2_mas_cercano/n_poligonos_involucrados,
        tiempo_promedio_clues_N3_mas_cercano=tiempo_promedio_clues_N3_mas_cercano/n_poligonos_involucrados
      )
    if(n_poligonos_involucrados>10){
      data_c_geo=data_c_geo|>dplyr::select(-NOMGEO)|>dplyr::mutate(NOMGEO=paste0(n_poligonos_involucrados," AGEBs y localidades"))
    }
    ##Se reutiliza el método de arriba con este polígono nuevo.
    AccesibilidadPoligono(data_c_geo)
  })
  observeEvent(input$filtrarPublicoPrivado,{
    req(selected_tab() == "map")
    memoriaPublicosPrivados$modal_open <- TRUE
    shinyalert(
      html = TRUE,
      text = tagList(
        div(class = "shinyalert-checkboxes",
            checkboxInput("publicas_", label = "Públicas", value = memoriaPublicosPrivados$publicos),
            checkboxInput("privadas_", label = "Privadas", value = memoriaPublicosPrivados$privados)
        )
      ),
      callbackR = function(value) {
        if(isTRUE(value)) { 
          if(!identical(c(
            input$nivel_at,input$publicas_,input$privadas_
          ),
                       c(
                         memoriaPublicosPrivados$nivel_at,memoriaPublicosPrivados$publicos,memoriaPublicosPrivados$privados
                       ))){
            memoriaPublicosPrivados$actualizar=TRUE
          }
          else{memoriaPublicosPrivados$actualizar=FALSE}
          memoriaPublicosPrivados$nivel_at <- input$nivel_at
          memoriaPublicosPrivados$publicos <- input$publicas_
          memoriaPublicosPrivados$privados <- input$privadas_
        }
        memoriaPublicosPrivados$modal_open <- FALSE 
      },
      closeOnClickOutside = F,closeOnEsc = F,
      title = "Selecciona el tipo de CLUES",
      confirmButtonText = "Aceptar"
    )
  })
  observeEvent(input$nivel_at,{memoriaPublicosPrivados$nivel_at=input$nivel_at
  memoriaPublicosPrivados$actualizar=TRUE})

  observe({
    shinyjs::toggleState("filtrarPublicoPrivado", condition = selected_tab() == "map")
  })
  # Tour Guide Implementation
  observeEvent(input$start_tour, {
    introjs(session, 
            events = list(
              onbeforechange = I(
                paste0(
                  "
      const step = targetElement.getAttribute('data-step');
      if (step === '4' || step === '5' || step === '6') {
        $('a[data-value=\"stats\"]').trigger('click');
      } else if (step === '7') {
        $('a[data-value=\"infra\"]').trigger('click');
      } else {
        $('a[data-value=\"map\"]').trigger('click');
      }
      "
                )
              )
            ),
            
            options = list(
              steps = data.frame(
                element = c(
                  "#tour_step_2_nivel",
                  "#tour_step_3_agebs",
                  "#tour_step_1_map",
                  "#tour_step_4_slider",
                  "#tour_step_5_download",
                  "#tour_step_6_table",
                  "#tour_step_7_infra"
                ),
                intro = c(
                  "<b>Seleccionar Nivel de Atención</b><br/>Elige entre 1er, 2do, 3er nivel o todos los niveles de CLUES para visualizar en el mapa. Esta elección define la accesibilidad en minutos de cada AGEB/localidad. <br/> <h3 style='color: #AE8E5D;'>Nuevo:</h3> Puedes filtrar por tipo de CLUES (Públicos y/o Privados) usando el botón de filtro.",
                  "<b>Agregar AGEBs y Localidades</b><br/>Activa esta opción para añadir datos demográficos de AGEBs y localidades al mapa. ",
                  "<b>Mapa Principal e Interactividad</b><br/> *: Puedes dar click en un CLUES para conocer información de accesibilidad (tiempo en minutos alrededor) y demográfica (población). <br> *: Da click a un polígono para concer la información de accesibilidad (Hospital más cercano y número de CLUES por tipo y rango de tiempo) y demográficas (Población total y afiliada a SS). <br> **: También puedes utilizar la herramienta de dibujo para seleccionar varios AGEBs y obtener un resumen. ",
                  "<b>Filtrar por Tiempo de Accesibilidad</b><br/>Usa este deslizador para seleccionar un tiempo en minutos. El sistema filtrará las localidades que tienen una accesibilidad en minutos mayor al valor seleccionado. El valor por defécto de 58 corresponde al indicador de accesibilidad de CONEVAL (2010) 'Tiempo promedio de traslado al hospital la última vez que se tuvo una emergencia'  ",
                  "<b>Descargar Datos</b><br/>Descarga los datos filtrados por tiempo en diferentes formatos (XLSX para municipios/localidades, GeoJSON para AGEBs).",
                  "<b>Tabla de Desglose</b><br/>Visualiza los datos detallados por municipios, localidades o AGEBs. Los datos se actualizan automáticamente según el tiempo seleccionado.",
                  "<b>Explora la infraestructura de salud disponible para CLUES del sector Público. Agrega filtros según el catálogo de SINERHIAS para visualizar los CLUES que cumplen con las condiciones seleccionadas."
                )
              )
            ))
  })
  ##Atemporales
  observeEvent(input$mapa_principal_draw_all_features,{
    req(selected_tab() == "map")
    if(length(input$mapa_principal_draw_all_features$features) == 0){
      lista_objetos_especiales(0)
    } else {
      lista_objetos_especiales(1)
    }
  })
  observe({
    if(lista_objetos_especiales() == 0){
      shinyjs::runjs(code =funcionColorearBotonBorrar("remove") )
    }
    else{
      shinyjs::runjs(code =funcionColorearBotonBorrar("add") )
    }
  })
  tabStatsServer("tab_stats", nivel_at = reactive(input$nivel_at), selected_tab = reactive(input$sidebarID))
  tabInfraServer("tab_infra", nivel_at = reactive(input$nivel_at), selected_tab = reactive(input$sidebarID), clues_en_operacion = clues_en_operacion, sinerhias = sinerhias)

})

#shiny::runApp("app.R",host = "0.0.0.0", port = 80)