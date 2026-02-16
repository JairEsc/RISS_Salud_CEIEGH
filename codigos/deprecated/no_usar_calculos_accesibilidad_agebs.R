##Documentamos el cálculo de opciones N1 y N2 para cada AGEB
demograficos_scince=st_read("outputs/demograficos_info_accesibilidad_cluesN1.geojson")
demograficos_scince=demograficos_scince[!(demograficos_scince$CVEGEO |> duplicated()),]##Error mío. Interrumpí un proceso y luego lo reinicié sin borrar los registros anteriores.

matriz_revision=matrix(c(NA,1,NA,1,1,1,NA,1,NA),ncol=3,nrow=3,byrow = T)
limites_municipales="../../Reutilizables/Cartografia/LIM_MUNICIPALES.shp" |> st_read()
cluesN1=(clues_en_operacion|> dplyr::filter(NIVEL.ATENCION=='PRIMER NIVEL') |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
           dplyr::mutate(geometry=st_as_sfc(geometry)) ) |> st_as_sf()
cluesN1=cluesN1 |> st_intersection(limites_municipales |> st_transform(4326)) |> dplyr::select(CLUES,geometry)

tiempo_zona=accCost(T.GC, matrix(cluesN1$geometry|> st_transform(st_crs("EPSG:32614") ) |> unlist() ,nrow = nrow(cluesN1),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=NA
tiempo_zona[ (tiempo_zona)>120]=NA
for(i in 1:100){
  tiempo_zona=focal(tiempo_zona, w =matriz_revision, fun = function(x) {if (all(is.na(x))) NA else mean(x, na.rm = TRUE)} , na.policy = "only")
  #inflamos hasta que cubra a los agebs.
  calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
  if((is.nan(calc_interseccion_cobertura) |> sum() )==0){
    print(i)#i=
    break
  }
}
# leaflet() |> addTiles() |> addPolygons(data=demograficos_scince) |>
#   addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = "Spectral",opacity = 0.4,group = "Accesibilidad en minutos")
#calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
demograficos_scince$tiempo_promedio_CLUES_N1=calc_interseccion_cobertura

cluesN2=(clues_en_operacion|> dplyr::filter(NIVEL.ATENCION=='SEGUNDO NIVEL') |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
           dplyr::mutate(geometry=st_as_sfc(geometry)) ) |> st_as_sf()
tiempo_zona=accCost(T.GC, matrix(cluesN2$geometry|> st_transform(st_crs("EPSG:32614") ) |> unlist() ,nrow = nrow(cluesN2),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=NA
tiempo_zona[ (tiempo_zona)>240]=NA
plot(tiempo_zona)
for(i in 1:100){
  tiempo_zona=focal(tiempo_zona, w =matriz_revision, fun = function(x) {if (all(is.na(x))) NA else mean(x, na.rm = TRUE)} , na.policy = "only")
  #inflamos hasta que cubra a los agebs.
  calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
  if((is.nan(calc_interseccion_cobertura) |> sum() )==0){
    print(i)#i=
    break
  }
}

#calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
demograficos_scince$tiempo_promedio_CLUES_N2=calc_interseccion_cobertura

cluesN3=(clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=='TERCER NIVEL') |> dplyr::select(CLUES,geometry)  |> dplyr::collect() |> 
           dplyr::mutate(geometry=st_as_sfc(geometry)) ) 
tiempo_zona=accCost(T.GC, matrix(cluesN3$geometry|> st_transform(st_crs("EPSG:32614") ) |> unlist() ,nrow = nrow(cluesN3),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=NA
tiempo_zona[ (tiempo_zona)>300]=NA
plot(tiempo_zona)
for(i in 1:100){
  tiempo_zona=focal(tiempo_zona, w =matriz_revision, fun = function(x) {if (all(is.na(x))) NA else mean(x, na.rm = TRUE)} , na.policy = "only")
  #inflamos hasta que cubra a los agebs.
  calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
  if((is.nan(calc_interseccion_cobertura) |> sum() )==0){
    print(i)#i=
    break
  }
}
plot(tiempo_zona)
#calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
demograficos_scince$tiempo_promedio_CLUES_N3=calc_interseccion_cobertura

cluesN=(clues_en_operacion |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
           dplyr::mutate(geometry=st_as_sfc(geometry)) ) |> st_as_sf()|> st_intersection(limites_municipales |> st_transform(4326)) |> dplyr::select(CLUES,geometry)
tiempo_zona=accCost(T.GC, matrix(cluesN$geometry|> st_transform(st_crs("EPSG:32614") ) |> unlist() ,nrow = nrow(cluesN),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=NA
tiempo_zona[ (tiempo_zona)>120]=NA
plot(tiempo_zona)
for(i in 1:100){
  tiempo_zona=focal(tiempo_zona, w =matriz_revision, fun = function(x) {if (all(is.na(x))) NA else mean(x, na.rm = TRUE)} , na.policy = "only")
  #inflamos hasta que cubra a los agebs.
  calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
  if((is.nan(calc_interseccion_cobertura) |> sum() )==0){
    print(i)#i=
    break
  }
}
#calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
demograficos_scince$tiempo_promedio_CLUES=calc_interseccion_cobertura



demograficos_scince=demograficos_scince |> dplyr::mutate(tiempo_promedio_CLUES_c=ifelse(tiempo_promedio_CLUES<10,"0-10",
                                                                                        ifelse(tiempo_promedio_CLUES<20,"10-20",
                                                                                               ifelse(tiempo_promedio_CLUES<40,"20-40",
                                                                                                      ifelse(tiempo_promedio_CLUES<60,"40-60",
                                                                                                             "+60")))))|> 
  dplyr::mutate(tiempo_promedio_CLUES_N1_c=ifelse(tiempo_promedio_CLUES_N1<10,"0-10",
                                                  ifelse(tiempo_promedio_CLUES_N1<20,"10-20",
                                                         ifelse(tiempo_promedio_CLUES_N1<40,"20-40",
                                                                ifelse(tiempo_promedio_CLUES_N1<60,"40-60",
                                                                       "+60"))))) |> 
  dplyr::mutate(tiempo_promedio_CLUES_N2_c=ifelse(tiempo_promedio_CLUES_N2<10,"0-10",
                                                  ifelse(tiempo_promedio_CLUES_N2<20,"10-20",
                                                         ifelse(tiempo_promedio_CLUES_N2<40,"20-40",
                                                                ifelse(tiempo_promedio_CLUES_N2<60,"40-60",
                                                                       "+60")))))
## CLUES nivel 2 más cercano(s)
clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="SEGUNDO NIVEL") |> 
  dplyr::select(geometry,CLUES,NOMBRE.DE.LA.UNIDAD) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
#demograficos_scince=demograficos_scince |> st_join(clues_solicitados |> st_as_sf(),join = st_nearest_feature)

demograficos_scince2=demograficos_scince |> dplyr::relocate(tiempo_promedio_CLUES_N3,.after = tiempo_promedio_CLUES_N2)
demograficos_scince2 |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/demograficos_info_accesibilidad_provisional.geojson",driver='GeoJSON',append=F,delete_dsn = T)
demograficos_scince1=st_read("outputs/demograficos_info_accesibilidad.geojson")
demograficos_scince2=st_read("outputs/demograficos_info_accesibilidad_provisional.geojson")##El provisional está más completo
