#En este se calcula para cada CLUES 
##Clave, Nombre de unidad, Nombre de localidad donde se ubica, Nombre de municipio donde se ubica (*)
#Pob Total en rango de 10 minutos 
#Pob Total afiliada en rango de 10 minutos 
#Pob Total en rango de 60 minutos 
#Pob Total afiliada en rango de 60 minutos
#Num CLUES nivel 1 en rango de 10 minutos
#Num CLUES nivel 2 en rango de 10 minutos
#Nombre de CLUES nivel 2 más cercano
#Accesibilidad en tiempo de CLUES nivel 2 más cercano

##Variables de capacidad de atención

clues_N2=(clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=='SEGUNDO NIVEL') |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry)) )

distancia_entre_cluesN2=gdistance::costDistance(T.GC,fromCoords = 
                          matrix(unlist(clues_N2$geometry|> st_transform(32614) ),nrow = nrow(clues_N2),ncol = 2,byrow = T)
                        ,toCoords = 
                          matrix(unlist(clues_N2$geometry |> st_transform(32614)),nrow = nrow(clues_N2),ncol = 2,byrow = T)
)

# Cada renglón contiene un cero, nos interesa el que sea distinto de sí mismo
distancia_entre_cluesN2 |> diag()##La diagonal es cero. 
distancia_entre_cluesN2 |> isSymmetric()
clues_N2_mas_cercanos=numeric(ncol(distancia_entre_cluesN2))
tiempos_N2_mas_cercanos=numeric(ncol(distancia_entre_cluesN2))
for(j in 1:ncol(distancia_entre_cluesN2)){
  which_min=append(distancia_entre_cluesN2[c(1:ncol(distancia_entre_cluesN2))[-j],j],values = Inf,after = (j-1) ) |> which.min()
  clues_N2_mas_cercanos[j]=which_min
  #clues_N2$CLUESN2_mas_cercana=clues_N2$CLUES[which_min]
  #clues_N2$tiempo_promedio_CLUES_N2[j]=distancia_entre_cluesN2[j,which_min]
  tiempos_N2_mas_cercanos[j]=distancia_entre_cluesN2[j,which_min]
}
clues_N2$CLUESN2_mas_cercana=clues_N2$CLUES[clues_N2_mas_cercanos]
clues_N2$tiempo_promedio_CLUES_N2=tiempos_N2_mas_cercanos
clues_N2$num_CLUESN2T10=(distancia_entre_cluesN2<10) |> rowSums()

##############Ahora incluimos agebs y pobs_rurales para calcular poblaciones
###Desde el raster. 
#Haces extract de valores con intersección simple. 
#Si un ageb tiene valor promedio NAN es porque no tiene intersección y se desecha. 
#Con los agebs que te quedas, ya puedes hacer las estimaciones de población 


clues_N2$POB1_T10=numeric(nrow(clues_N2))
clues_N2$POB1_T60=numeric(nrow(clues_N2))
clues_N2$SALUD1_T10=numeric(nrow(clues_N2))
clues_N2$SALUD1_T60=numeric(nrow(clues_N2))
for(i in 1:nrow(clues_N2)){
  if(i%%10==0) print(i)
  
  clueN2=clues_N2[i,]
  #print(clueN2)
  isocronas_niveles_fijos=accCost(T.GC, clues_N2$geometry[i] |> st_transform(st_crs("EPSG:32614") ) |> unlist() )
  isocronas_niveles_fijos[isocronas_niveles_fijos>61]=NA
  isocronas_niveles_fijos[!is.na(isocronas_niveles_fijos)]=1
  isocronas_niveles_fijos[is.na(isocronas_niveles_fijos)]=0
  #print(isocronas_niveles_fijos)
  calc_interseccion_cobertura=exactextractr::exact_extract(x = isocronas_niveles_fijos,y = demograficos_scince |> st_transform(32614),'frac')
  
  sumas_poblaciones=demograficos_scince |> st_drop_geometry() |> dplyr::mutate(POB1=POB1*calc_interseccion_cobertura$frac_1,
                                       SALUD1=SALUD1*calc_interseccion_cobertura$frac_1
                                       ) |> dplyr::group_by(CVEGEO) |> dplyr::summarise(
                                         POB1=sum(POB1,na.rm=T),
                                         SALUD1=sum(SALUD1,na.rm=T)
                                                                                        ) |> 
    dplyr::filter(POB1>0) |> dplyr::ungroup() |> dplyr::summarise(
      POB1=sum(POB1,na.rm=T),
      SALUD1=sum(SALUD1,na.rm=T)
    )
  
  clues_N2$POB1_T60[i]=  sumas_poblaciones$POB1 |> sum(na.rm = T)

  clues_N2$SALUD1_T60[i]=  sumas_poblaciones$SALUD1 |> sum(na.rm = T)

  isocronas_niveles_fijos=accCost(T.GC, clues_N2$geometry[i] |> st_transform(st_crs("EPSG:32614") ) |> unlist() )
  isocronas_niveles_fijos[isocronas_niveles_fijos>11]=NA
  isocronas_niveles_fijos[!is.na(isocronas_niveles_fijos)]=1
  isocronas_niveles_fijos[is.na(isocronas_niveles_fijos)]=0
  #print(isocronas_niveles_fijos)
  calc_interseccion_cobertura=exactextractr::exact_extract(x = isocronas_niveles_fijos,y = demograficos_scince |> st_transform(32614),'frac')
  
  sumas_poblaciones=demograficos_scince |> st_drop_geometry() |> dplyr::mutate(POB1=POB1*calc_interseccion_cobertura$frac_1,
                                                                               SALUD1=SALUD1*calc_interseccion_cobertura$frac_1
  ) |> dplyr::group_by(CVEGEO) |> dplyr::summarise(
    POB1=sum(POB1,na.rm=T),
    SALUD1=sum(SALUD1,na.rm=T)
  ) |> 
    dplyr::filter(POB1>0) |> dplyr::ungroup() |> dplyr::summarise(
      POB1=sum(POB1,na.rm=T),
      SALUD1=sum(SALUD1,na.rm=T)
    )
  clues_N2$POB1_T10[i]=  sumas_poblaciones$POB1 |> sum(na.rm = T)

  clues_N2$SALUD1_T10[i]=  sumas_poblaciones$SALUD1 |> sum(na.rm = T)

}


##Vamos a guardar estos datos.
clues_N2 |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/cluesN2_info_accesibilidad.geojson",driver = "GeoJSON",append = F,delete_dsn = T)
limites_municipales="../../Reutilizables/Cartografia/LIM_MUNICIPALES.shp" |> st_read()
cluesN1=(clues_en_operacion|> dplyr::filter(NIVEL.ATENCION=='PRIMER NIVEL') |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry)) ) |> st_as_sf()
cluesN1=cluesN1 |> st_intersection(limites_municipales |> st_transform(4326)) |> dplyr::select(CLUES,geometry)
clues_N2=(clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=='SEGUNDO NIVEL') |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry)) )

distancia_entre_cluesN2=gdistance::costDistance(T.GC,fromCoords = 
                          matrix(unlist(cluesN1$geometry|> st_transform(32614) ),nrow = nrow(cluesN1)
                                 ,ncol = 2,byrow = T)
                        ,toCoords = 
                          matrix(unlist(clues_N2$geometry |> st_transform(32614)),nrow = nrow(clues_N2),ncol = 2,byrow = T)
)
####################################################
####################################################
####################################################
#Me quedé aquí. [No está calculando accesibilidades como debería. Caso N1 323 lo liga al N2 66 ! Ya quedó. Eran una clues de monterrey que hacian que fallara]
distancia_entre_cluesN2 |> diag()##La diagonal ya no es cero
clues_N2_mas_cercanos=numeric(nrow(distancia_entre_cluesN2))
tiempos_N2_mas_cercanos=numeric(nrow(distancia_entre_cluesN2))
for(j in 1:nrow(distancia_entre_cluesN2)){
  which_min=distancia_entre_cluesN2[j,] |> which.min()
  clues_N2_mas_cercanos[j]=which_min
  #cluesN1$CLUESN2_mas_cercana=clues_N2$CLUES[which_min]
  #cluesN1$tiempo_promedio_CLUES_N2[j]=distancia_entre_cluesN2[j,which_min]
  tiempos_N2_mas_cercanos[j]=distancia_entre_cluesN2[j,which_min]
}
cluesN1$CLUESN2_mas_cercana=clues_N2$CLUES[clues_N2_mas_cercanos]# clues_N2_mas_cercanos
cluesN1$tiempo_promedio_CLUES_N2=tiempos_N2_mas_cercanos
clues_N2$num_CLUESN1T10=(distancia_entre_cluesN2<10) |> colSums()

#########Agregamos poblaciones 
cluesN1$num_CLUESN2T10=(distancia_entre_cluesN2<10) |> rowSums()


cluesN1$POB1_T10=numeric(nrow(cluesN1))
cluesN1$POB1_T60=numeric(nrow(cluesN1))
cluesN1$SALUD1_T10=numeric(nrow(cluesN1))
cluesN1$SALUD1_T60=numeric(nrow(cluesN1))
for(i in 40:nrow(cluesN1)){
  if(i%%10==0) print(i)
  #i=40
  clueN1=cluesN1[i,]
  #print(clueN2)
  isocronas_niveles_fijos=accCost(T.GC, cluesN1$geometry[i] |> st_transform(st_crs("EPSG:32614") ) |> unlist() )
  if(isocronas_niveles_fijos |> values() |> unique() |> length()<3){
    print("El punto está fuera del área:")
    print(paste0("i=",i))
    print(cluesN1$geometry[i])
    #crop(terra::rast("inputs/rasters/1er_N_Privado_caminando_max_90.tif"))
    isocronas_niveles_fijos=accCost(T.GC, terra::rast("inputs/rasters/1er_N_Privado_caminando_max_90.tif") |> crop(cluesN1[i,] |> st_as_sf() |> st_buffer(1000) |> st_transform(32614)) |> terra::as.points() |> st_as_sf() |> dplyr::mutate(dist=st_distance(geometry,cluesN1[i,] |> st_as_sf() |> st_buffer(1000) |> st_transform(32614))) |> dplyr::arrange(dplyr::desc(dist)) |> dplyr::slice_head(n=1) |> st_geometry() |> unlist() ) 
  }
  isocronas_niveles_fijos[isocronas_niveles_fijos>61]=NA
  isocronas_niveles_fijos[!is.na(isocronas_niveles_fijos)]=1
  isocronas_niveles_fijos[is.na(isocronas_niveles_fijos)]=0
  #print(isocronas_niveles_fijos)
  calc_interseccion_cobertura=exactextractr::exact_extract(x = isocronas_niveles_fijos,y = demograficos_scince |> st_transform(32614),'frac')
  if(ncol(calc_interseccion_cobertura) |> is.null()){
    filtro_demograficos=demograficos_scince  |> dplyr::filter(st_distance(geometry,cluesN1[i,] |> st_as_sf()) |> as.numeric()<1000)
  }
  else{
    filtro_demograficos=demograficos_scince |> st_drop_geometry() |> dplyr::mutate(POB1=POB1*calc_interseccion_cobertura$frac_1,
                                                                                   SALUD1=SALUD1*calc_interseccion_cobertura$frac_1
    )
  }
  sumas_poblaciones=filtro_demograficos |> dplyr::group_by(CVEGEO) |> dplyr::summarise(
    POB1=sum(POB1,na.rm=T),
    SALUD1=sum(SALUD1,na.rm=T)
  ) |> 
    dplyr::filter(POB1>0) |> dplyr::ungroup() |> dplyr::summarise(
      POB1=sum(POB1,na.rm=T),
      SALUD1=sum(SALUD1,na.rm=T)
    )
  
  cluesN1$POB1_T60[i]=  sumas_poblaciones$POB1 |> sum(na.rm = T)
  
  cluesN1$SALUD1_T60[i]=  sumas_poblaciones$SALUD1 |> sum(na.rm = T)
  
  isocronas_niveles_fijos=accCost(T.GC, cluesN1$geometry[i] |> st_transform(st_crs("EPSG:32614") ) |> unlist() )
  if(isocronas_niveles_fijos |> values() |> unique() |> length()<3){
    print("El punto está fuera del área:")
    print(paste0("i=",i))
    print(cluesN1$geometry[i])
    #crop(terra::rast("inputs/rasters/1er_N_Privado_caminando_max_90.tif"))
    isocronas_niveles_fijos=accCost(T.GC, terra::rast("inputs/rasters/acces_CLUES_max90.tif") |> crop(cluesN1[i,] |> st_as_sf() |> st_buffer(1000) |> st_transform(32614)) |> terra::as.points() |> st_as_sf() |> dplyr::mutate(dist=st_distance(geometry,cluesN1[i,] |> st_as_sf() |> st_buffer(1000) |> st_transform(32614))) |> dplyr::arrange(dplyr::desc(dist)) |> dplyr::slice_head(n=1) |> st_geometry() |> unlist() ) 
  }
  isocronas_niveles_fijos[isocronas_niveles_fijos>11]=NA
  isocronas_niveles_fijos[!is.na(isocronas_niveles_fijos)]=1
  isocronas_niveles_fijos[is.na(isocronas_niveles_fijos)]=0
  #print(isocronas_niveles_fijos)
  calc_interseccion_cobertura=exactextractr::exact_extract(x = isocronas_niveles_fijos,y = demograficos_scince |> st_transform(32614),'frac')
  if(ncol(calc_interseccion_cobertura) |> is.null()){
    filtro_demograficos=demograficos_scince  |> dplyr::filter(st_distance(geometry,cluesN1[i,] |> st_as_sf()) |> as.numeric()<1000)
  }
  else{
    filtro_demograficos=demograficos_scince |> st_drop_geometry() |> dplyr::mutate(POB1=POB1*calc_interseccion_cobertura$frac_1,
                                                               SALUD1=SALUD1*calc_interseccion_cobertura$frac_1
    )
  }
  
  sumas_poblaciones=filtro_demograficos |> dplyr::group_by(CVEGEO) |> dplyr::summarise(
    POB1=sum(POB1,na.rm=T),
    SALUD1=sum(SALUD1,na.rm=T)
  ) |> 
    dplyr::filter(POB1>0) |> dplyr::ungroup() |> dplyr::summarise(
      POB1=sum(POB1,na.rm=T),
      SALUD1=sum(SALUD1,na.rm=T)
    )
  cluesN1$POB1_T10[i]=  sumas_poblaciones$POB1 |> sum(na.rm = T)
  
  cluesN1$SALUD1_T10[i]=  sumas_poblaciones$SALUD1 |> sum(na.rm = T)
  
}

##Faltaría nomás calcular el número de CLUESN1 a menos de 10 minutos para cada cluesN1.
distancia_entre_cluesN1=gdistance::costDistance(T.GC,fromCoords = 
                                                  matrix(unlist(cluesN1$geometry|> st_transform(32614) ),nrow = nrow(cluesN1)
                                                         ,ncol = 2,byrow = T)
                                                ,toCoords = 
                                                  matrix(unlist(cluesN1$geometry |> st_transform(32614)),nrow = nrow(cluesN1),ncol = 2,byrow = T)
)
cluesN1$num_CLUESN1T10=(distancia_entre_cluesN1<10) |> rowSums()
##Vamos a guardar estos clues más cercanos
cluesN1 |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> dplyr::mutate(tiempo_promedio_CLUES_N2=ifelse((tiempo_promedio_CLUES_N2)>400,400,tiempo_promedio_CLUES_N2) ) |> 
  st_write("outputs/cluesN1_info_accesibilidad.geojson",driver = "GeoJSON",append = F,delete_dsn = T)
