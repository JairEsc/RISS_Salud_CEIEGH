#
excel_ruta="inputs/Variables SINERHIAS_0526 Niveles.xlsx"
capacidad_instalada=excel_ruta |> openxlsx::loadWorkbook() |> openxlsx::sheets() |> lapply(\(sh){
 openxlsx::read.xlsx(excel_ruta, sh,startRow = 7)
})
capacidad_instalada=capacidad_instalada |> setNames(excel_ruta |> openxlsx::loadWorkbook() |> openxlsx::sheets() )
catalogo="inputs/Variables SINERHIAS_0526 Catalogo.xlsx" |> openxlsx::read.xlsx(sheet = "Variables",skipEmptyCols = T,check.names = T) 

clues_en_operacion=openxlsx::read.xlsx("outputs/confidenciales/ESTABLECIMIENTO_SALUD_202604.xlsx")
clues_en_operacion=clues_en_operacion |> 
  dplyr::filter(CLAVE.DE.LA.ENTIDAD=='13') 
clues_en_operacion=clues_en_operacion |> 
  dplyr::mutate(LATITUD=as.numeric(LATITUD),
                LONGITUD=as.numeric(LONGITUD))
clues_en_operacion=clues_en_operacion |> 
  dplyr::filter(LONGITUD<0 & LATITUD>0) |> 
  st_as_sf(coords = c("LONGITUD","LATITUD"),crs=4326)
capacidad_instalada_N1=capacidad_instalada[[1]]
##cero_en_todas_partes
((capacidad_instalada_N1[,4:ncol(capacidad_instalada_N1)] |> rowSums(na.rm = T))>0) |> all()#no es cero en todas partes

capacidad_instalada_N1_tidy=capacidad_instalada_N1 |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  dplyr::group_by(NombreVar) |>
  tidyr::pivot_longer(cols = HGDIF000043:HGPMX000035,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))|> 
  dplyr::mutate(across(where(is.numeric), as.integer))|> 
  merge(clues_en_operacion |> dplyr::select(CLUES,geometry),by='CLUES',all.x=T) |> 
  dplyr::filter(!is.na(geometry))
capacidad_instalada_N2=capacidad_instalada[[2]]
capacidad_instalada_N2_tidy=capacidad_instalada_N2 |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  dplyr::group_by(NombreVar)|> tidyr::pivot_longer(cols = HGIMB000151:HGPMX000016,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))|> 
  dplyr::mutate(across(where(is.numeric), as.integer))|> 
  merge(clues_en_operacion |> dplyr::select(CLUES,geometry),by='CLUES',all.x=T) |> 
  dplyr::filter(!is.na(geometry))

capacidad_instalada_N3=capacidad_instalada[[3]]
capacidad_instalada_N3_tidy=capacidad_instalada_N3 |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  dplyr::group_by(NombreVar)|> tidyr::pivot_longer(cols = HGDIF000014:HGIMB002304,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))|> 
  dplyr::mutate(across(where(is.numeric), as.integer))|> 
  merge(clues_en_operacion |> dplyr::select(CLUES,geometry),by='CLUES',all.x=T) |> 
  dplyr::filter(!is.na(geometry))

####faltantes
ncol(capacidad_instalada_N1[,4:ncol(capacidad_instalada_N1)])-
  nrow(capacidad_instalada_N1_tidy)
ncol(capacidad_instalada_N2[,4:ncol(capacidad_instalada_N2)])-
  nrow(capacidad_instalada_N2_tidy)
ncol(capacidad_instalada_N3[,4:ncol(capacidad_instalada_N3)])-
  nrow(capacidad_instalada_N3_tidy)


source("../../Reutilizables/Postgres_BUIG/conexion_local.R")
local=DBI::dbConnect(RSQLite::SQLite(), "clues_demograficos_municipios.sqlite")
sinerhias <- DBI::dbConnect(RSQLite::SQLite(), "outputs/confidenciales/clues_SINERHIAS_int.sqlite")
clues_en_operacion=st_read(local,"clues_en_operacion")
##Falta agregar datos calculados a los geojsons
library(sf)

st_write(capacidad_instalada_N1_tidy |> 
           dplyr::mutate(NIVEL.ATENCION='PRIMER NIVEL'), sinerhias, "N1_CLUES_SINERHIAS", delete_layer = T)
st_write(capacidad_instalada_N2_tidy |> 
           dplyr::mutate(NIVEL.ATENCION='SEGUNDO NIVEL'), sinerhias, "N2_CLUES_SINERHIAS", delete_layer = T)
st_write(capacidad_instalada_N3_tidy |> 
           dplyr::mutate(NIVEL.ATENCION='TERCER NIVEL'), sinerhias, "N3_CLUES_SINERHIAS", delete_layer = T)

capacidades_instaladas=do.call(plyr::rbind.fill,capacidad_instalada[1:3])
capacidades_instaladas=capacidad_instalada[[1]] |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  merge(capacidad_instalada[[2]] |> 
          dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) ,by='NombreVar',all=T) |> 
  merge(capacidad_instalada[[3]] |> 
          dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) ,by='NombreVar',all=T) 
capacidades_instaladas_tidy=capacidades_instaladas |> 
  dplyr::group_by(NombreVar) |> tidyr::pivot_longer(cols = HGDIF000043:HGIMB002304,names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))
capacidades_instaladas_tidy=capacidades_instaladas_tidy |> 
  dplyr::mutate(dplyr::across(where(is.numeric),~tidyr::replace_na(., 0))) |> 
  dplyr::mutate(dplyr::across(where(is.numeric), as.integer))|> merge(clues_en_operacion |> dplyr::select(CLUES,geometry),by='CLUES',all.x=T) |> 
  dplyr::filter(!is.na(geometry))
capacidades_instaladas_tidy=capacidades_instaladas_tidy |> 
  dplyr::rowwise() |> 
  dplyr::mutate(NIVEL.ATENCION=
                  ifelse(CLUES%in%capacidad_instalada_N1_tidy$CLUES,"PRIMER NIVEL",
                         ifelse(CLUES%in%capacidad_instalada_N2_tidy$CLUES,"SEGUNDO NIVEL","TERCER NIVEL"))) |> 
  dplyr::relocate(NIVEL.ATENCION,.after = CLUES) |> 
  dplyr::ungroup() |> 
  st_as_sf()
st_write(capacidades_instaladas_tidy, sinerhias, "CLUES_SINERHIAS", delete_layer = T,append=F)


###
catalogo=catalogo |> 
  merge(y =capacidad_instalada_N1 |> dplyr::select(NombreVar) |> dplyr::mutate(primer_nivel=1) ,by='NombreVar',all.x=T) |> 
  merge(y =capacidad_instalada_N2 |> dplyr::select(NombreVar) |> dplyr::mutate(segundo_nivel=1) ,by='NombreVar',all.x=T) |> 
  merge(y =capacidad_instalada_N3 |> dplyr::select(NombreVar) |> dplyr::mutate(tercer_nivel=1) ,by='NombreVar',all.x=T) |> 
  dplyr::mutate(cualquier_nivel=ifelse(sum(c(primer_nivel,segundo_nivel,tercer_nivel),na.rm=T)>0,1,0))#####Pendiente 
catalogo=catalogo|> dplyr::mutate(across(where(is.numeric), as.integer)) |> 
  dplyr::mutate(dplyr::across(where(is.numeric),~tidyr::replace_na(., 0)))
st_write(catalogo, sinerhias, "catalogo", delete_layer = T,append=F)
DBI::dbDisconnect(sinerhias)
library(leaflet)
library(gdistance)

generarMapaWeb = function(nivel_de_atencion = 'N1_', variable1) {
  nombre_tabla = paste0(nivel_de_atencion, "CLUES_SINERHIAS")
  clues_filtrados = dplyr::tbl(sinerhias, nombre_tabla) |> 
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
generarMapaWeb("N2",variable1 = "expedienteclinico")

