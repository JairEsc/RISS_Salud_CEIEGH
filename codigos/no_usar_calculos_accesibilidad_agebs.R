##Documentamos el cálculo de opciones N1 y N2 para cada AGEB
demograficos_scince=st_read("outputs/demograficos_cartograficos_scince_20.geojson")


limites_municipales="../../Reutilizables/Cartografia/LIM_MUNICIPALES.shp" |> st_read()
cluesN1=(clues_en_operacion|> dplyr::filter(NIVEL.ATENCION=='PRIMER NIVEL') |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
           dplyr::mutate(geometry=st_as_sfc(geometry)) ) |> st_as_sf()
cluesN1=cluesN1 |> st_intersection(limites_municipales |> st_transform(4326)) |> dplyr::select(CLUES,geometry)

tiempo_zona=accCost(T.GC, matrix(cluesN1$geometry|> st_transform(st_crs("EPSG:32614") ) |> unlist() ,nrow = nrow(cluesN1),ncol = 2,byrow = T))
tiempo_zona[ is.infinite(tiempo_zona)]=200
tiempo_zona[ (tiempo_zona)>200]=200

calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
demograficos_scince$tiempo_promedio_CLUES_N1=calc_interseccion_cobertura

cluesN2=(clues_en_operacion|> dplyr::filter(NIVEL.ATENCION=='SEGUNDO NIVEL') |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
           dplyr::mutate(geometry=st_as_sfc(geometry)) ) |> st_as_sf()
tiempo_zona=accCost(T.GC, matrix(cluesN2$geometry|> st_transform(st_crs("EPSG:32614") ) |> unlist() ,nrow = nrow(cluesN2),ncol = 2,byrow = T))
tiempo_zona[ is.infinite(tiempo_zona)]=200
tiempo_zona[ (tiempo_zona)>200]=200

calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
demograficos_scince$tiempo_promedio_CLUES_N2=calc_interseccion_cobertura

cluesN=(clues_en_operacion |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
           dplyr::mutate(geometry=st_as_sfc(geometry)) ) |> st_as_sf()|> st_intersection(limites_municipales |> st_transform(4326)) |> dplyr::select(CLUES,geometry)
tiempo_zona=accCost(T.GC, matrix(cluesN$geometry|> st_transform(st_crs("EPSG:32614") ) |> unlist() ,nrow = nrow(cluesN),ncol = 2,byrow = T))
tiempo_zona[ is.infinite(tiempo_zona)]=200
tiempo_zona[ (tiempo_zona)>200]=200

calc_interseccion_cobertura=exactextractr::exact_extract(x = tiempo_zona ,y = demograficos_scince |> st_transform(32614),'mean')
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
demograficos_scince=demograficos_scince |> st_join(clues_solicitados |> st_as_sf(),join = st_nearest_feature)

demograficos_scince |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/demograficos_info_accesibilidad.geojson",driver='GeoJSON',append=F,delete_dsn = T)
