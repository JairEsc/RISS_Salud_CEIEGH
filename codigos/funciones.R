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
  
  for (i in seq(nrow(isochrones_sf), 2)) {
    isochrones_sf$geometry[i] = sf::st_difference(isochrones_sf[i, ],isochrones_sf[i - 1, ])$geometry
  }
  
  isochrones_sf = isochrones_sf |> 
    dplyr::mutate(geometry = sf::st_make_valid(geometry))
  
  # Check isochrone polygons
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






