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

sinerhias_n1_3=capacidad_instalada[1:3] |> 
  lapply(\(capacidad_instalada){
    capacidad_instalada |> dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
      dplyr::group_by(NombreVar) |>
      tidyr::pivot_longer(cols = dplyr::starts_with("H"),names_to = "CLUES") |> 
      tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))|> 
      dplyr::mutate(across(where(is.numeric), as.integer))|> 
      merge(clues_en_operacion |> dplyr::select(CLUES,NOMBRE.DE.LA.INSTITUCION,MUNICIPIO,LOCALIDAD,NOMBRE.DE.LA.UNIDAD,geometry),by='CLUES',all.x=T) |> 
      dplyr::filter(!is.na(geometry))
  })
sinerhias_n1_3=sinerhias_n1_3 |> purrr::map2(c("primer","segundo","tercer"), 
  \(clues,nivel){
    equipamientos=clues |> dplyr::select(-CLUES,-NOMBRE.DE.LA.INSTITUCION,
                                         -MUNICIPIO,-LOCALIDAD,-NOMBRE.DE.LA.UNIDAD,
                                         -geometry)|> 
      st_drop_geometry()
    cbind(clues,equipamiento= round(equipamientos|> rowSums()/ncol(equipamientos),3) ) |> 
      dplyr::relocate(equipamiento,.before = geometry) |> 
      dplyr::mutate(NIVEL.ATENCION=paste(stringr::str_to_upper(nivel),"NIVEL") ) |> 
      dplyr::relocate(NIVEL.ATENCION,.before = geometry)
  }
)


sinerhias <- DBI::dbConnect(RSQLite::SQLite(), "outputs/confidenciales/clues_SINERHIAS_int.sqlite")
#clues_en_operacion=st_read(local,"clues_en_operacion")
catalogo_sql=dplyr::tbl(sinerhias,"catalogo") |> dplyr::collect()

st_write(sinerhias_n1_3[[1]], sinerhias, "N1_CLUES_SINERHIAS", delete_layer = T)
st_write(sinerhias_n1_3[[2]], sinerhias, "N2_CLUES_SINERHIAS", delete_layer = T)
st_write(sinerhias_n1_3[[3]], sinerhias, "N3_CLUES_SINERHIAS", delete_layer = T)

capacidades_instaladas=capacidad_instalada[[1]] |> 
  dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) |> 
  merge(capacidad_instalada[[2]] |> 
          dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) ,by='NombreVar',all=T) |> 
  merge(capacidad_instalada[[3]] |> 
          dplyr::select(-Descripcion.de.la.variable,-TipoDato.Especifico) ,by='NombreVar',all=T) 
capacidades_instaladas_tidy=capacidades_instaladas |> 
  dplyr::group_by(NombreVar) |> tidyr::pivot_longer(cols = dplyr::starts_with("H"),names_to = "CLUES") |> 
  tidyr::pivot_wider(names_from = c(NombreVar),values_from = c(value))
capacidades_instaladas_tidy=capacidades_instaladas_tidy |> 
  dplyr::mutate(dplyr::across(where(is.numeric),~tidyr::replace_na(., 0))) |> 
  dplyr::mutate(dplyr::across(where(is.numeric), as.integer))|> merge(clues_en_operacion |> 
                                                                        dplyr::select(CLUES,NOMBRE.DE.LA.INSTITUCION,MUNICIPIO,LOCALIDAD,NOMBRE.DE.LA.UNIDAD,geometry),by='CLUES',all.x=T) |> 
  dplyr::filter(!is.na(geometry))
capacidades_instaladas_tidy=capacidades_instaladas_tidy |> 
  dplyr::rowwise() |> 
  dplyr::mutate(NIVEL.ATENCION=
                  ifelse(CLUES%in%sinerhias_n1_3[[1]]$CLUES,"PRIMER NIVEL",
                         ifelse(CLUES%in%sinerhias_n1_3[[2]]$CLUES,"SEGUNDO NIVEL","TERCER NIVEL"))) |> 
  dplyr::relocate(NIVEL.ATENCION,.after = CLUES) |> 
  dplyr::ungroup() |> 
  st_as_sf()
st_write(capacidades_instaladas_tidy, sinerhias, "CLUES_SINERHIAS", delete_layer = T,append=F)


###
catalogo=catalogo |> 
  merge(y =capacidad_instalada[[1]] |> dplyr::select(NombreVar) |> dplyr::mutate(primer_nivel=1) ,by='NombreVar',all.x=T) |> 
  merge(y =capacidad_instalada[[2]] |> dplyr::select(NombreVar) |> dplyr::mutate(segundo_nivel=1) ,by='NombreVar',all.x=T) |> 
  merge(y =capacidad_instalada[[3]] |> dplyr::select(NombreVar) |> dplyr::mutate(tercer_nivel=1) ,by='NombreVar',all.x=T) |> 
  dplyr::mutate(cualquier_nivel=ifelse(sum(c(primer_nivel,segundo_nivel,tercer_nivel),na.rm=T)>0,1,0))#####Pendiente 
catalogo=catalogo|> dplyr::mutate(across(where(is.numeric), as.integer)) |> 
  dplyr::mutate(dplyr::across(where(is.numeric),~tidyr::replace_na(., 0)))

c("N1_CLUES_SINERHIAS" ,"N2_CLUES_SINERHIAS","N3_CLUES_SINERHIAS") |> lapply(\(nivel){
  purrr::map2(catalogo$NombreVar, rep(nivel,184), 
              \(nombreVar,nivel){
                totales=dplyr::tbl(sinerhias,nivel) |> dplyr::collect()
                if(!nombreVar%in% colnames(totales)){
                  return(NA)
                }
                disponibles= totales |> 
                  dplyr::filter(!!dplyr::sym(nombreVar)>0 ) |> nrow()
                return(disponibles)
              })             
})->z
z |> lapply(unlist)->zzz

catalogo=catalogo |> cbind(disponibilidad_primer_nivel=zzz[[1]])
catalogo=catalogo |> cbind(disponibilidad_segundo_nivel=zzz[[2]])
catalogo=catalogo |> cbind(disponibilidad_tercer_nivel=zzz[[3]])
catalogo$disponibilidad_cualquier_nivel=catalogo |> dplyr::select(disponibilidad_primer_nivel:disponibilidad_tercer_nivel) |> rowSums(na.rm = T)

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


