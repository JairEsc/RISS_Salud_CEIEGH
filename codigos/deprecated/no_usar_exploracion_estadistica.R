###
#Datos generales: 
#Número de personas / porcentaje con al menos un clues n1 a menos de 10 minutos
#Número de personas / porcentaje con al menos un clues n2 a menos de 10 minutos
#Número de personas / porcentaje con al menos un clues n1 a menos de 60 minutos
#Número de personas / porcentaje con al menos un clues n2 a menos de 60 minutos

#Localidades donde no se cumpla, visualizables y filtrables/ descargables
#Municipios donde no se cumpla, visualizables y filtrables/ descargables

#Por municipio, tiempo promedio de accesibilidad a clues n1 y n2. Escalado por poblacion

#Por localidades, tiempo promedio de accesibilidad a clues n1 y n2. Escalado por poblacion. Filtrable y descargable

#Por clues, promedio de personas a menos de 10 y 60 minutos, filtrable y descargable

#agebs con menos de k clues a menos de 10 minutos
demograficos_scince |> dplyr::filter(as.numeric(CLUES_N1_10)<1) |> nrow()
#agebs con menos de k clues a menos de 60 minutos
demograficos_scince |> dplyr::filter(as.numeric(CLUES_N1_60)<1) |> nrow()
#agebs con el clues N1 a más de k minutos
demograficos_scince |> dplyr::filter(as.numeric(tiempo_promedio_CLUES_N1)>60) |> nrow()
#agebs con el clues N1 a más de k minutos
demograficos_scince |> dplyr::filter(as.numeric(tiempo_promedio_CLUES_N2)>100) |> nrow()

#Puede ser a partir de un nivel de atención y un tiempo
#N_k, T 
#Un select de nivel de atención y un slider de tiempo desde 10 a 90

##Conteo de población con acceso a CLUES N_k a más de T minutos
##Conteo de población por municipio con acceso a CLUES _n_k a más de T minutos
##Conteo de población por localidad con acceso a CLUES _n_k a más de T minutos

estadisticas_dado_nivel_atencion_y_tiempo=function(nivel,tiempo){
  # tiempo=60
  # nivel=2
  lista_agebs=demograficos_scince |> 
    dplyr::select(
      CVEGEO, POB1, SALUD1, NOM_MUN, NOMGEO, 
      tiempo_promedio_CLUES:tiempo_promedio_CLUES_N2
    ) 
  if(nivel%in%c('PRIMER NIVEL',1) ){
    lista_agebs=lista_agebs|> 
    dplyr::filter(tiempo_promedio_CLUES_N1 > tiempo)
  }else{
    lista_agebs=lista_agebs|> 
      dplyr::filter(tiempo_promedio_CLUES_N2 > tiempo)
  }
  lista_agebs=lista_agebs |> 
    dplyr::mutate(POB_rel=round(100*POB1/sum(POB1,na.rm=T),1))
  conteo_por_localidad=lista_agebs |> 
    sf::st_drop_geometry() |> 
    dplyr::group_by(NOM_MUN, NOMGEO) |> 
    dplyr::summarise(
      num_agebs = dplyr::n(),
      POB1   = sum(ifelse(POB1 < 0, NA, POB1), na.rm = TRUE),
      SALUD1 = sum(ifelse(SALUD1 < 0, NA, SALUD1), na.rm = TRUE), 
      dplyr::across(
        starts_with("tiempo_promedio_CLUES"), 
        \(x) mean(x, na.rm = TRUE)
      )
    )
  conteo_por_municipio=conteo_por_localidad |> 
    dplyr::ungroup() |> 
    dplyr::group_by(NOM_MUN) |> 
    dplyr::summarise(
      num_locs = dplyr::n(),
      POB1   = sum(ifelse(POB1 < 0, NA, POB1), na.rm = TRUE),
      SALUD1 = sum(ifelse(SALUD1 < 0, NA, SALUD1), na.rm = TRUE), 
      dplyr::across(
        starts_with("tiempo_promedio_CLUES"), 
        \(x) mean(x, na.rm = TRUE)
      )
    )
  return(list(lista_agebs,conteo_por_localidad,conteo_por_municipio))
}
zz=estadisticas_dado_nivel_atencion_y_tiempo(nivel = 2,tiempo = 60)
##los agebs en un mapa coloreado por población y una tabla
##las localidades en un mapa coloreado por población y una tabla
##los municipios en un mapa coloreado por población y una tabla

##Totales: 
  #Poblacion total
  #Poblacion afiliada
  #Número de municipios
  #Número de localidades

leaflet() |> addTiles() |> addPolygons(data=zz[[1]],label = zz[[1]]$CVEGEO) |> 
  addMarkers(data=clues_en_operacion |> dplyr::collect() |> dplyr::mutate(geometry=st_as_sfc(geometry)) |> st_as_sf())