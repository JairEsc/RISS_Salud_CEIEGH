#
excel_ruta="inputs/Variables SINERHIAS_0526 Niveles.xlsx"
capacidad_instalada=excel_ruta |> openxlsx::loadWorkbook() |> openxlsx::sheets() |> lapply(\(sh){
 openxlsx::read.xlsx(excel_ruta, sh,startRow = 7)
})
capacidad_instalada=capacidad_instalada |> setNames(excel_ruta |> openxlsx::loadWorkbook() |> openxlsx::sheets() )
catalogo="inputs/Variables SINERHIAS_0526 Catalogo.xlsx" |> openxlsx::read.xlsx(sheet = "Variables",skipEmptyCols = T,check.names = T) 

capacidad_instalada_N1=capacidad_instalada[[1]]
##cero_en_todas_partes
((capacidad_instalada_N1[,4:ncol(capacidad_instalada_N1)] |> rowSums(na.rm = T))>0) |> all()#no es cero en todas partes

capacidad_instalada_N1_tidy=capacidad_instalada_N1 |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  dplyr::group_by(NombreVar) |>
  tidyr::pivot_longer(cols = HGDIF000043:HGPMX000035,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))
capacidad_instalada_N2=capacidad_instalada[[2]]
capacidad_instalada_N2_tidy=capacidad_instalada_N2 |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  dplyr::group_by(NombreVar)|> tidyr::pivot_longer(cols = HGIMB000151:HGPMX000016,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))
capacidad_instalada_N3=capacidad_instalada[[3]]
capacidad_instalada_N3_tidy=capacidad_instalada_N3 |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  dplyr::group_by(NombreVar)|> tidyr::pivot_longer(cols = HGDIF000014:HGIMB002304,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))

##Ejemplo de consulta
capacidad_instalada_N1_tidy |> 
  dplyr::filter(ambulancia>0) |> 
  dplyr::select(CLUES)


source("../../Reutilizables/Postgres_BUIG/conexion_local.R")
con <- DBI::dbConnect(RSQLite::SQLite(), "outputs/confidenciales/clues_SINERHIAS.sqlite")
clues_en_operacion=st_read(local,"clues_en_operacion")
##Falta agregar datos calculados a los geojsons
library(sf)
st_write(capacidad_instalada_N1_tidy|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),all.x=T), con, "N1_CLUES_SINERHIAS", delete_layer = FALSE)
st_write(capacidad_instalada_N2_tidy|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),all.x=T), con, "N2_CLUES_SINERHIAS", delete_layer = FALSE)
st_write(capacidad_instalada_N3_tidy|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),all.x=T), con, "N3_CLUES_SINERHIAS", delete_layer = FALSE)

catalogo=catalogo |> 
  merge(y =capacidad_instalada_N1 |> dplyr::select(NombreVar) |> dplyr::mutate(primer_nivel=1) ,by='NombreVar',all.x=T) |> 
  merge(y =capacidad_instalada_N2 |> dplyr::select(NombreVar) |> dplyr::mutate(segundo_nivel=1) ,by='NombreVar',all.x=T) |> 
  merge(y =capacidad_instalada_N3 |> dplyr::select(NombreVar) |> dplyr::mutate(tercer_nivel=1) ,by='NombreVar',all.x=T) |> 
  dplyr::mutate(cualquier_nivel=ifelse(sum(c(primer_nivel,segundo_nivel,tercer_nivel),na.rm=T)>0,1,0))#####Pendiente 
st_write(catalogo, con, "catalogo", delete_layer = FALSE)

library(leaflet)
library(gdistance)

generarMapaWeb = function(nivel_de_atencion = 'N1_', variable1) {
  nombre_tabla = paste0(nivel_de_atencion, "CLUES_SINERHIAS")
  clues_filtrados = dplyr::tbl(con, nombre_tabla) |> 
    dplyr::filter(dplyr::if_any(dplyr::matches(variable1), ~ .x > 0)) |> 
    dplyr::collect() |> 
    dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
  lugares_ficticios=clues_filtrados |> st_transform(32614)
  tiempo_zona = gdistance::accCost(T.GC, matrix(unlist(lugares_ficticios$geometry),nrow = nrow(lugares_ficticios),ncol = 2,byrow = T))  # Calcula el costo acumulado desde un punto de inicio (coordenadas especificadas) usando la matriz de transición corregida (T.GC).
  tiempo_zona[tiempo_zona>90]=NA
  crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
  
  leaflet() |> 
    addTiles() |> 
    addMarkers(data = clues_filtrados) |> 
    addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = "Spectral",group = "Accesibilidad carretera (en minutos)") 
}
capacidad_instalada_N1_tidy |> colnames()
generarMapaWeb("N2",variable1 = "expedienteclinico")

capacidades_instaladas=do.call(plyr::rbind.fill,capacidad_instalada[1:3])
capacidades_instaladas_tidy=capacidades_instaladas |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  dplyr::group_by(NombreVar) |> dplyr::slice_head(n=1) |> tidyr::pivot_longer(cols = HGDIF000043:HGIMB002304,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))
st_write(capacidades_instaladas_tidy|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),all.x=T), con, "CLUES_SINERHIAS", delete_layer = FALSE)
