
##################################################
#Pendiente#
generadorPopUpContentDemog=function(poligono){
  return(
    paste0(
      "<strong>Clave geográfica:</strong> ", 
      ((poligono$CVEGEO))
      ,"<br>",
      "<strong>Municipio:</strong> ", 
      ((poligono$NOM_MUN))
      ,"<br>",
      "<strong>Localidad:</strong> ", 
      ((poligono$NOMGEO))
      ,"<br>",
      "<strong>Población Estimada:</strong> ", 
      format(round(poligono$POB1, 0), big.mark = ","), " habitantes"
      ,"<br>",
      "<strong>Población Afiliada a SS:</strong> ", 
      format(round(poligono$SALUD1, 0), big.mark = ","), " habitantes"
      ,"<br>",
      "<strong>Tiempo promedio a CLUES de nivel 1 más cercano:</strong> ", 
      format(round(poligono$tiempo_promedio_CLUES_N1, 1), big.mark = ","), " minutos "
      ,"<br>",
      "<strong>Tiempo promedio a CLUES de nivel 2 más cercano:</strong> ", 
      format(round(poligono$tiempo_promedio_CLUES_N2, 1), big.mark = ","), " minutos"
      ,"<br>",
      "<strong>CLUES nivel 2 más cercano: </strong> ", 
      paste0(poligono$CLUES," - ",poligono$NOMBRE.DE.LA.UNIDAD), ""
    )
  )
}
generadorPopUpContentCLUES=function(poligono){
  return(
    paste0(
      "<strong>CLUES:</strong> ", 
      ((poligono$CLUES))
      ,"<br>",
      "<strong>Municipio:</strong> ", 
      ((poligono$MUNICIPIO))
      ,"<br>",
      "<strong>Localidad:</strong> ", 
      ((poligono$LOCALIDAD))
      ,"<br>",
      "<strong>Población Estimada a menos de 10 minutos:</strong> ", 
      format(round(poligono$POB1_T10, 0), big.mark = ","), " habitantes"
      ,"<br>",
      "<strong>Población Estimada a menos de 60 minutos:</strong> ", 
      format(round(poligono$POB1_T60, 0), big.mark = ","), " habitantes"
      ,"<br>",
      "<strong>Población Afiliada a SS a menos de 10 minutos:</strong> ", 
      format(round(poligono$SALUD1_T10, 0), big.mark = ","), " habitantes"
      ,"<br>",
      "<strong>Población Afiliada a SS a menos de 60 minutos:</strong> ", 
      format(round(poligono$SALUD1_T60, 0), big.mark = ","), " habitantes"
      ,"<br>",
      "<strong>Número de CLUES de nivel 2 a menos de 10 minutos:</strong> ", 
      format(round(poligono$num_CLUESN2T10, 1), big.mark = ","), " unidades"
      #  ,"<br>", #########Pendientes
      # "<strong>Tiempo promedio a CLUES de nivel 2 más cercano:</strong> ", 
      # format(round(poligono$tiempo_promedio_CLUES_N2, 1), big.mark = ","), " minutos"
      #  ,"<br>",
      # "<strong>CLUES nivel 2 más cercano: </strong> ", 
      # paste0(poligono$CLUES," - ",poligono$NOMBRE.DE.LA.UNIDAD), ""
    )
  )
}

#Cuando la información censal esn confidencial (-6 o -8), modificar el formato para reflejarlo
AccesibilidadPoligono=function(poligono,leaflet_proxy='mapa_principal'){
  centroide=st_centroid(poligono)
  print("centroide:")
  
  ##Clave, Nombre de localidad, Nombre de municipio. 
  #Pob Total. Pob Afiliada. Piramide poblacional. Distribución de afiliación. 
  ##Tiempo promedio a CLUES de nivel 1
  ##Tiempo promedio a CLUES de nivel 2 y nombre de la más cercana
  popup_content <- generadorPopUpContentDemog(poligono)
  print(poligono)
  if(poligono$POB1==0){
    popup_content<-"Asegúrate de que la capa de AGEBs y localidades rurales esté activa y que la región seleccionada por ti contenga al menos uno de ellos"
  }
  coords <- sf::st_coordinates(centroide)
  leaflet::leafletProxy(leaflet_proxy) |> 
    leaflet::addPopups(
      lng = coords[1, "X"], # Longitud
      lat = coords[1, "Y"], # Latitud
      popup = popup_content,
      options = leaflet::popupOptions(closeButton = TRUE,closeOnClick = F)
    )
  return(0)

}
AccesibilidadCLUES=function(poligono,centro,leaflet_proxy='mapa_principal'){
  centroide=centro
  ##Clave, Nombre de unidad, Nombre de localidad donde se ubica, Nombre de municipio donde se ubica
  #Pob Total en rango de 10 minutos 
  #Pob Total afiliada en rango de 10 minutos 
  #Pob Total en rango de 60 minutos 
  #Pob Total afiliada en rango de 60 minutos
  #Num CLUES nivel 1 en rango de 10 minutos
  #Num CLUES nivel 2 en rango de 10 minutos
    #Nombre de CLUES nivel 2 más cercano
  ##Variables de capacidad de atención
  popup_content <- generadorPopUpContentCLUES(poligono)
  #print(poligono)
  coords <- sf::st_coordinates(centroide)
  leaflet::leafletProxy(leaflet_proxy) |> 
    leaflet::addPopups(
      lng = coords[1, "X"], # Longitud
      lat = coords[1, "Y"], # Latitud
      popup = popup_content,
      options = leaflet::popupOptions(closeButton = TRUE,closeOnClick = F)
    )
  return(0)

}

###############
estadisticas_dado_nivel_atencion_y_tiempo=function(nivel,tiempo){
  # tiempo=60
  # nivel=2
  lista_agebs=demograficos_scince |> 
    dplyr::select(NOM_MUN,NOMGEO,CVEGEO,POB1,POB42,POB84,SALUD1,tiempo_promedio_CLUES:tiempo_promedio_CLUES_N3,CLUES,NOMBRE.DE.LA.UNIDAD,CLUES_N1_10,CLUES_N1_60)
    
  if(nivel%in%c('PRIMER NIVEL',1) ){
    lista_agebs=lista_agebs|> 
      dplyr::filter(tiempo_promedio_CLUES_N1 > tiempo)
  }else if(nivel%in%c('SEGUNDO NIVEL',2)){
    lista_agebs=lista_agebs|> 
      dplyr::filter(tiempo_promedio_CLUES_N2 > tiempo)
  } else if(nivel%in%c('TERCER NIVEL',3)){
    lista_agebs=lista_agebs|> 
      dplyr::filter(tiempo_promedio_CLUES_N3 > tiempo)
  } else{lista_agebs=lista_agebs|> 
    dplyr::filter(tiempo_promedio_CLUES > tiempo)}
  lista_agebs=lista_agebs 
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
  
  
  lista_agebs=lista_agebs|> 
    dplyr::mutate(POB_rel=round(100*POB1/sum(POB1,na.rm=T),1)) |> 
    dplyr::arrange(dplyr::desc(POB_rel)) |> 
    dplyr::mutate(
      tiempo_promedio_CLUES_N1=round(tiempo_promedio_CLUES_N1,1),
      tiempo_promedio_CLUES_N2=round(tiempo_promedio_CLUES_N2,1),
      tiempo_promedio_CLUES_N3=round(tiempo_promedio_CLUES_N3,1),
      tiempo_promedio_CLUES=round(tiempo_promedio_CLUES,1)
    )
  conteo_por_localidad=conteo_por_localidad |> 
    dplyr::arrange(dplyr::desc(POB1)) |> 
    dplyr::select(NOM_MUN,NOMGEO,POB1,SALUD1,tiempo_promedio_CLUES:tiempo_promedio_CLUES_N3) |> 
    dplyr::mutate(
      tiempo_promedio_CLUES_N1=round(tiempo_promedio_CLUES_N1,1),
      tiempo_promedio_CLUES_N2=round(tiempo_promedio_CLUES_N2,1),
      tiempo_promedio_CLUES_N3=round(tiempo_promedio_CLUES_N3,1),
      tiempo_promedio_CLUES=round(tiempo_promedio_CLUES,1)
    )
  conteo_por_municipio=conteo_por_municipio|> 
    dplyr::arrange(dplyr::desc(POB1)) |> 
    dplyr::select(NOM_MUN,POB1,SALUD1,tiempo_promedio_CLUES:tiempo_promedio_CLUES_N3) |> 
    dplyr::mutate(
      tiempo_promedio_CLUES_N1=round(tiempo_promedio_CLUES_N1,1),
      tiempo_promedio_CLUES_N2=round(tiempo_promedio_CLUES_N2,1),
      tiempo_promedio_CLUES_N3=round(tiempo_promedio_CLUES_N3,1),
      tiempo_promedio_CLUES=round(tiempo_promedio_CLUES,1)
    )
  popupsContents=generadorPopUpContentDemog(lista_agebs)
  return(list(lista_agebs,conteo_por_localidad,conteo_por_municipio,popupsContents))
}

