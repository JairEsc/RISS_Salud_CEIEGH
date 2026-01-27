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
union_clues |> write.csv("inputs/UNIDADES_SALUD_HGO.csv",row.names = F,fileEncoding = "UTF-8")

clues_fuera_operacion=union_clues |> 
  dplyr::filter(archivo_origen=="CLUES_202509 (Fuera_Operación)") |> 
  dplyr::filter(LATITUD>10) |> 
  sf::st_as_sf(coords=c('LONGITUD','LATITUD'),crs=4326)
clues_en_operacion_s=union_clues |> 
  dplyr::filter(archivo_origen!="CLUES_202509 (Fuera_Operación)") |> 
  sf::st_as_sf(coords=c('LONGITUD','LATITUD'),crs=4326)


##Aquí va el código para actualizar la tabla en postgres
clues_en_operacion_s=clues_en_operacion_s |> 
  dplyr::filter(NIVEL.ATENCION!='NO APLICA')

#1009+188+2

#st_write(clues_en_operacion_s,dsn = local,layer = "clues_en_operacion",append = F,delete_dsn = T)

#st_read(local,'clues_en_operacion')
#dplyr::tbl(local,"clues_en_operacion") |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
#  dplyr::mutate(geometry=st_as_sfc(geometry)) |> st_as_sf()

