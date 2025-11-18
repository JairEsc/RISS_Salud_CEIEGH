"inputs/UNIDADES_SALUD_HGO.xlsx" |> openxlsx::getSheetNames() |> 
  lapply(\(z){
    openxlsx::read.xlsx(xlsxFile = "inputs/UNIDADES_SALUD_HGO.xlsx",sheet = z) |> 
      dplyr::mutate(archivo_origen=z)
  })->clues
sheets_clues="inputs/UNIDADES_SALUD_HGO.xlsx" |> openxlsx::getSheetNames()

zz=clues[[1]]
clues |> sapply(\(zz){
  zz |> colnames()
}) |> unlist()  |> sort()


do.call(plyr::rbind.fill,clues)->union_clues


clues_fuera_operacion=union_clues |> 
  dplyr::filter(archivo_origen=="CLUES_202509 (Fuera_Operación)") |> 
  dplyr::filter(LATITUD>10) |> 
  sf::st_as_sf(coords=c('LONGITUD','LATITUD'),crs=4326)
clues_en_operacion=union_clues |> 
  dplyr::filter(archivo_origen!="CLUES_202509 (Fuera_Operación)") |> 
  sf::st_as_sf(coords=c('LONGITUD','LATITUD'),crs=4326)


leaflet::leaflet() |> 
  leaflet::addTiles() |> 
  leaflet::addMarkers(data=clues_en_operacion,label = clues_en_operacion$NOMBRE.DE.LA.INSTITUCION,clusterOptions = leaflet::markerClusterOptions())
