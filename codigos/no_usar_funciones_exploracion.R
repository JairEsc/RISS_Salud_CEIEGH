library(httr)
getIsochrones_mapbox=function(coord,times=c(5,15,25,35)){
  iso.url <- paste("https://api.mapbox.com/isochrone/v1/mapbox/driving/",
                   coord,
                   "?contours_minutes=",paste(times,collapse = ","),"&polygons=true&access_token=",Sys.getenv("token_mapbox"),sep = "")
  
  #GET()# Compile each individual catchment area polygon
  r = httr::GET(url = iso.url)
  geojson_txt = content(r, as = "text", encoding = "UTF-8")
  isochrones_sf = sf::st_read(geojson_txt, quiet = TRUE)
  isochrones_sf = isochrones_sf |> 
    dplyr::select(contour) |> 
    dplyr::arrange(contour)
  
  # Add projection
  sf::st_crs(isochrones_sf) = 4326
  ##Pasó algo con este. Quizás actualizaron su API.
  ##sf::st_difference(isochrones_sf$geometry[4],isochrones_sf$geometry[3])[2] |> st_make_valid() |> plot()
  ##Esto es una solución, pero está raro.
  # for (i in seq(nrow(isochrones_sf), 2)) {
  #   isochrones_sf$geometry[i] = sf::st_difference(isochrones_sf$geometry[i ],isochrones_sf$geometry[i - 1 ])[2]
  # }
  # 
  # isochrones_sf = isochrones_sf |>
  #   dplyr::mutate(geometry = sf::st_make_valid(geometry))
  
  return(isochrones_sf)
}

interseccion_optima = function(datos1, datos2){
  
  datos1 = datos1 |> 
    dplyr::rename(tiempo = dplyr::any_of("contour"))
  
  datos2 = datos2 |> 
    dplyr::rename(tiempo = dplyr::any_of("contour"))
  
  
  interseccion = sf::st_intersection(x = datos1, y = datos2) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(tiempo = min(tiempo, tiempo.1)) |> 
    dplyr::select(tiempo) |> dplyr::arrange(tiempo) |> 
    dplyr::mutate(geometry = sf::st_make_valid(geometry))
  
  interseccion_junto = sf::st_union(interseccion) |>  sf::st_as_sf() |> sf::st_make_valid()
  
  datos1_act = datos1 |>  sf::st_difference(y = interseccion_junto) |>  sf::st_collection_extract(type = "POLYGON", warn = F) 
  datos2_act = datos2 |>  sf::st_difference(y = interseccion_junto) |>  sf::st_collection_extract(type = "POLYGON", warn = F)
  
  
  datos = dplyr::bind_rows(datos1_act, datos2_act, interseccion) |> 
    dplyr::mutate(geometry = sf::st_make_valid(geometry)) |> 
    dplyr::arrange(tiempo)
  
  rownames(datos) = NULL
  
  return(datos)
}


### Ignorar por el momento ########################
interseccion = function(datos1, datos2){
  
  ### Posible linea de conflicto, pensar
  datos1 = datos1 |>  
    dplyr::group_by(contour) |>
    dplyr::summarise(geometry = sf::st_union(geometry)) |>
    dplyr::ungroup() |> 
    dplyr::mutate(geometry = sf::st_make_valid(geometry))
  
  datos2 = datos2 |>  
    dplyr::group_by(contour) |>
    dplyr::summarise(geometry = sf::st_union(geometry)) |>
    dplyr::ungroup() |> 
    dplyr::mutate(geometry = sf::st_make_valid(geometry))
  
  #####################################
  
  interseccion = sf::st_intersection(x = datos1, y = datos2)
  interseccion = interseccion |> 
    dplyr::rowwise() |> 
    dplyr::mutate(tiempo = min(contour, contour.1)) |> 
    dplyr::select(tiempo)
  
  interseccion_junto = sf::st_union(interseccion) |>  sf::st_as_sf() |> sf::st_make_valid()
  
  datos1_act = datos1 |> sf::st_make_valid() |>  sf::st_difference(y = interseccion_junto) |>  sf::st_collection_extract(type = "POLYGON", warn = F) 
  datos2_act = datos2 |> sf::st_make_valid() |>  sf::st_difference(y = interseccion_junto) |>  sf::st_collection_extract(type = "POLYGON", warn = F)
  
  datos1_act = datos1_act |> 
    dplyr::rename(tiempo = contour) |> 
    dplyr::mutate(proviene = "datos1")
  
  datos2_act = datos2_act |> 
    dplyr::rename(tiempo = contour) |> 
    dplyr::mutate(proviene = "datos2")
  
  interseccion = interseccion |> 
    dplyr::mutate(proviene = "interseccion")
  
  datos = dplyr::bind_rows(datos1_act, datos2_act, interseccion) |> 
    dplyr::mutate(geometry = sf::st_make_valid(geometry)) |> 
    dplyr::rename(contour = tiempo)
  
  return(datos)
}
##################################################


AccesibilidadPoligono=function(poligono){
  centroide=st_centroid(poligono)
  print("centroide:")
  print(centroide$geometry |> unlist() |> paste(collapse = ","))
  isocronas_niveles_fijos=getIsochrones_mapbox(coord = centroide$geometry |> unlist() |> paste(collapse = ","),
                                               times =c(10,20,40,60) ) |> st_as_sf() |> st_transform(st_crs("EPSG:4326"))
  
  conteo_por_nivel_y_rangoT=isocronas_niveles_fijos |> 
    st_join(y = clues_en_operacion |> dplyr::select(NIVEL.ATENCION,geometry) |> 
              dplyr::collect() |> dplyr::mutate(geometry=st_as_sfc(geometry)) |> 
              st_as_sf(),join = st_intersects) |> 
    dplyr::group_by(NIVEL.ATENCION,contour) |> 
    st_drop_geometry()|> 
    dplyr::summarise(conteo=dplyr::n()) |> dplyr::arrange(dplyr::desc(contour)) |> dplyr::ungroup()###Nota. Dado que las isocronas son concentricas,  
  #se puede (y debe) inferir las clues por rango disjunto
  
  #Parte de gráficos
  N=(conteo_por_nivel_y_rangoT |> dplyr::group_by(NIVEL.ATENCION) |> dplyr::slice_head(n=1))$conteo |> sum()
  N_s=(conteo_por_nivel_y_rangoT |> dplyr::group_by(NIVEL.ATENCION) |> dplyr::slice_head(n=1))$conteo
  print("--------")
  print(N_s[0])##Traigo un bug. Esto debería ser vector
  niveles=(conteo_por_nivel_y_rangoT |> dplyr::group_by(NIVEL.ATENCION) |> dplyr::slice_head(n=1))$NIVEL.ATENCION
  print(niveles[0])
  ##Lo ponemos así en feo. 
  print(
    paste0("Esta comunidad tiene ",N," CLUES a menos de 60 minutos, de las cuales ",N_s[0]," son de ",niveles[0])
  )
  return(0)
}



