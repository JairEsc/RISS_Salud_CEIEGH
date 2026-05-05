#
catalogo="inputs/Catalogo_SINERHIAS_2025 F.xlsx" |> openxlsx::read.xlsx(startRow = 9,sheet = "CATALOGO_SINERHIAS_2024",skipEmptyCols = T,check.names = T) 
capacidad_instalada="inputs/Variables SINERHIAS_UPLAPH.xlsx" |> openxlsx::loadWorkbook() |> openxlsx::sheets() |> lapply(\(sh){
 openxlsx::read.xlsx("inputs/Variables SINERHIAS_UPLAPH.xlsx", sh,startRow = 7)
})
capacidad_instalada=capacidad_instalada |> setNames("inputs/Variables SINERHIAS_UPLAPH.xlsx" |> openxlsx::loadWorkbook() |> openxlsx::sheets() )
capacidad_instalada_N1=capacidad_instalada[[1]] |> 
  dplyr::mutate(Variable=
                  iconv(x  = stringr::str_to_upper( Variable),from = "UTF-8",to="ASCII//TRANSLIT"))

capacidad_instalada_N1$Variable |> unique()
catalogo_N1=catalogo |> 
  dplyr::mutate(Descripcion.de.la.variable=
                  iconv(x  = stringr::str_to_upper( Descripcion.de.la.variable),from = "UTF-8",to="ASCII//TRANSLIT"))

catalogo_N1=catalogo_N1 |> 
  dplyr::filter(Descripcion.de.la.variable
                %in%
                  capacidad_instalada_N1$Variable)

(!(capacidad_instalada_N1$Variable |> lapply(\(col){
  col%in%catalogo_N1$Descripcion.de.la.variable
}) |> unlist() )) |> which()->faltantes
capacidad_instalada_N1$Variable[faltantes]##Hay varias que no estaban en el catálogo de variables. 



capacidad_instalada_N1=capacidad_instalada[[1]]
##cero_en_todas_partes
((capacidad_instalada_N1[,2:ncol(capacidad_instalada_N1)] |> rowSums(na.rm = T))>0) |> all()#no es cero en todas partes

##No nos sirve en este formato. 
##Hay variables repetidas con valores diferentes entre CLUES
#e.g. 
1:1 |> lapply(\(n){
  capacidad_instalada[[1]] |> tidyr::pivot_longer(cols = HGDIF000043:HGPMX000035,##dplyr::any_of
                                                  names_to = "CLUES") |>
    dplyr::summarise(n = dplyr::n(), .by = c(CLUES, Variable)) |>
    dplyr::filter(n > 1L)  |> dplyr::select(Variable) |> unique()
}) 

capacidad_instalada_N1=capacidad_instalada[[1]]
capacidad_instalada_N1_tidy=capacidad_instalada_N1 |> 
  dplyr::group_by(Variable) |> 
  dplyr::slice_head(n=1)|> tidyr::pivot_longer(cols = HGDIF000043:HGPMX000035,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(Variable),values_from = c(value))
capacidad_instalada_N2=capacidad_instalada[[2]]
capacidad_instalada_N2_tidy=capacidad_instalada_N2 |> 
  dplyr::group_by(Variable) |> 
  dplyr::slice_head(n=1)|> tidyr::pivot_longer(cols = HGIMB000151:HGPMX000016,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(Variable),values_from = c(value))
capacidad_instalada_N3=capacidad_instalada[[3]]
capacidad_instalada_N3_tidy=capacidad_instalada_N3 |> 
  dplyr::group_by(Variable) |> 
  dplyr::slice_head(n=1)|> tidyr::pivot_longer(cols = HGDIF000014:HGIMB002304,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(Variable),values_from = c(value))

##Ejemplo de consulta
capacidad_instalada_N1_tidy |> 
  dplyr::filter(AMBULANCIAS>0 & `NUMERO DE CAMAS EN AREA DE URGENCIAS`>0) |> 
  dplyr::select(CLUES)



##########
capacidades_instaladas="inputs/Variables SINERHIAS_UPLAPH.xlsx" |> openxlsx::loadWorkbook() |> openxlsx::sheets() |> 
  lapply(
    \(z){
      capacidad_instalada[[z]] |> dplyr::mutate(nivel_de_atencion=z )
    }
  )

source("../../Reutilizables/Postgres_BUIG/conexion_local.R")
con <- DBI::dbConnect(RSQLite::SQLite(), "outputs/clues_SINERHIAS.sqlite")
clues_en_operacion=st_read(local,"clues_en_operacion")
##Falta agregar datos calculados a los geojsons
library(sf)
st_write(capacidad_instalada_N1_tidy|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),all.x=T), con, "N1_CLUES_SINERHIAS", delete_layer = FALSE)
st_write(capacidad_instalada_N2_tidy|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),all.x=T), con, "N2_CLUES_SINERHIAS", delete_layer = FALSE)
st_write(capacidad_instalada_N3_tidy|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),all.x=T), con, "N3_CLUES_SINERHIAS", delete_layer = FALSE)

DBI::dbDisconnect(con)

con=DBI::dbConnect(RSQLite::SQLite(), "outputs/clues_SINERHIAS.sqlite")


library(leaflet)
library(gdistance)
generarMapaWeb = function(nivel_de_atencion = 'N1', variable1) {
  variable1='¿Cuenta con área de Laboratorio Clínico?'
  nivel_de_atencion='N1'
  nombre_tabla = paste0(nivel_de_atencion, "_CLUES_SINERHIAS")
  clues_filtrados = dplyr::tbl(con, nombre_tabla) |> 
    dplyr::filter(dplyr::if_any(dplyr::matches(variable1), ~ .x > 0)) |> 
    dplyr::collect() |> 
    dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
  lugares_ficticios=clues_filtrados |> st_transform(32614)
  tiempo_zona = accCost(T.GC, matrix(unlist(lugares_ficticios$geometry),nrow = nrow(lugares_ficticios),ncol = 2,byrow = T))  # Calcula el costo acumulado desde un punto de inicio (coordenadas especificadas) usando la matriz de transición corregida (T.GC).
  tiempo_zona[tiempo_zona>90]=NA
  crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
  
  leaflet() |> 
    addTiles() |> 
    addMarkers(data = clues_filtrados) |> 
    addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = "Spectral",group = "Accesibilidad carretera (en minutos)") 
}
generarMapaWeb("N1",variable1 = "¿Cuenta con área de Laboratorio Clínico?")

capacidades_instaladas=do.call(plyr::rbind.fill,capacidades_instaladas)
capacidades_instaladas=capacidades_instaladas |> 
  dplyr::relocate(nivel_de_atencion,.after = Variable)
capacidades_instaladas_tidy=capacidades_instaladas |> 
  dplyr::group_by(Variable,nivel_de_atencion) |> 
  dplyr::slice_head(n=1)|> tidyr::pivot_longer(cols = HGDIF000043:HGIMB002304,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(Variable),values_from = c(value))
