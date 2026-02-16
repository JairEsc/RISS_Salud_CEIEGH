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

clues_solicitados=clues_en_operacion |> #dplyr::filter(NIVEL.ATENCION=="PRIMER NIVEL") |> 
  dplyr::select(CLUES,NIVEL.ATENCION,geometry) |> dplyr::arrange(NIVEL.ATENCION) |> dplyr::collect() |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
T.GC=readRDS("inputs/accesibilidad_SIGEH/accesibilidad_carretera.rds")

costo_traslado_entre_clues=gdistance::costDistance(T.GC,fromCoords = 
                                                  matrix(unlist(clues_solicitados$geometry|> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T)
                                                ,toCoords = 
                                                  matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T)
)
costo_traslado_entre_clues=distancia_entre_clues
costo_traslado_entre_clues |> apply(MARGIN=1,FUN=\(row){
  row=costo_traslado_entre_clues[1,]
  n1=1002
  n2=188
  n3=2
  particion1_row=row[1:n1]
  particion2_row=row[(n1+1):(n1+n2)]
  particion3_row=row[(n1+n2+1):(n1+n2+n3)]
  T_s=c(10,20,40,60)
  conteo_clues_N1=T_s |> lapply(\(t){
    num_cluesN1_menorT=sum(particion1_row<t)#numero de clues a menos de T minutos
    num_cluesN2_menorT=sum(particion2_row<t)#numero de clues a menos de T minutos
    num_cluesN3_menorT=sum(particion3_row<t)#numero de clues a menos de T minutos
    return(
      list(
        num_cluesN1_menorT,num_cluesN2_menorT,num_cluesN3_menorT
        )
    )
    #guardar los índices del primero y segundo más cercanos. #Cuando el índice del renglón no cae en la misma partición, es más interesante
    
    })
  clues_minimo=which.min(particion1_row)
  tiempo_minimo=particion1_row[clues_minimo]
  particion1_row[clues_minimo]=Inf
  clues_minimo2=which.min(particion1_row)
  tiempo_minimo2=particion1_row[clues_minimo2]
  
  
  
})

###############################

idx <- 0
num_clues_por_tipo=clues_solicitados |> st_drop_geometry() |> dplyr::group_by(NIVEL.ATENCION) |> dplyr::summarise(conteo=dplyr::n())
num_clues_por_tipo$conteo
n1 = num_clues_por_tipo$conteo[1]
n2 = num_clues_por_tipo$conteo[2]
n3 = num_clues_por_tipo$conteo[3]
lista_ids=rangos_nivel <- list(clues_solicitados$CLUES[1:n1], clues_solicitados$CLUES[(n1+1):(n1+n2)], clues_solicitados$CLUES[(n1+n2+1):(n1+n2+n3)])
diag(costo_traslado_entre_clues)=Inf 

metricas_clues <- apply(costo_traslado_entre_clues, MARGIN = 1, FUN = function(row) {
  idx <<- idx + 1  
  #row=costo_traslado_entre_clues[157,]
  particiones_del_renglon=list(
    c1=row[1:n1],
    c2=row[(n1 + 1):(n1 + n2)],
    c3=row[(n1 + n2 + 1):(n1 + n2 + n3)]
  )
  T_s = c(10, 20, 40, 60)
  conteos_por_nivel <- lapply(particiones_del_renglon, \(sub_row) {
    lapply(T_s, function(t) sum(sub_row < t))
  })
  
  minimos_por_nivel <- lapply(1:3, function(k) {
    sub_row <- particiones_del_renglon[[k]]
    
    # Nivel 1: 1 a n1 | Nivel 2: n1+1 a n1+n2 | Nivel 3: n1+n2+1 a total
    rangos_nivel <- list(1:n1, (n1+1):(n1+n2), (n1+n2+1):(n1+n2+n3))
    
    # if (idx %in% rangos_nivel[[k]]) {#innecesario. Ponle Inf en la diagonal para más fácil
    #   # Si estoy en mi propio nivel, ignoro el 0 (distancia a mí mismo)
    #   # Buscamos la posición relativa dentro de la partición
    #   pos_relativa <- idx - min(rangos_nivel[[k]]) + 1
    #   sub_row[pos_relativa] <- Inf##Para no tomarlo como mínimo
    # }
    
    clues_minimo_idx=which.min(sub_row)
    tiempo_minimo=sub_row[clues_minimo_idx]
    
    return(list(id_cercano = lista_ids[[k]][clues_minimo_idx], tiempo = tiempo_minimo))
  })
  
  return(list(conteos = conteos_por_nivel, cercania = minimos_por_nivel))
})

metricas_clues_df=do.call(rbind,args = metricas_clues |> lapply(unlist)) |> as.data.frame()
metricas_clues_df$CLUES=clues_solicitados$CLUES
#Una disculpa por esto
colnames(metricas_clues_df)=c(paste0(paste0("Conteo_N",1:3 |> lapply(\(w)rep(w,4)) |> unlist()),"_T",rep(c(10,20,40,60),3)),
                              paste0(c("CLUES_N","Tiempo_promedio_CLUES_N") |> rep(3) |> unlist(),1:3 |> lapply(\(w){w|> rep(2)}) |> unlist(),"_mas_cercano"),
                              "CLUES"
)
metricas_clues_df |> dplyr::relocate(CLUES,.before = Conteo_N1_T10) |> write.csv("outputs/metricas_clues.csv",row.names = F)




#########Estimaciones de población por rangos de tiempo
testeo_isocronas_clues=function(){
  library(leaflet)
  leaflet() |> addTiles() |> addPolygons(data=demograficos_scince[424,]) |> 
    addMarkers(data=clue_aislado) |> 
    addRasterImage(projectRasterForLeaflet(isocronas_niveles_fijos,method = "ngb"),colors = "Spectral",group = "Accesibilidad carretera (en minutos)") 
}
lista_aportaciones_por_clues=list()
for(i in 1:nrow(clues_solicitados)){
  if(i%%100==0){print(i)}
  clue_aislado=clues_solicitados[i,]
  isocronas_niveles_fijos=gdistance::accCost(T.GC, clue_aislado$geometry |> st_transform(st_crs("EPSG:32614") ) |> unlist() )
  isocronas_niveles_fijos[isocronas_niveles_fijos>60]=NA
  #plot(isocronas_niveles_fijos)
  #crs(isocronas_niveles_fijos)=st_crs("EPSG:32614")$wkt

  calc_interseccion_cobertura=exactextractr::exact_extract(x = isocronas_niveles_fijos,y = demograficos_scince |> st_transform(32614)
  )
  simplificacion_interseccion_cobertura=(calc_interseccion_cobertura |> lapply(\(df){
    if(df$value |> is.na() |> all()){return(list(NA,NA,NA,NA))}
    else{
      ##Quiero la distribución por rango de valores: <10,<20,<40,<60
      return(list(
        ((df$value<10)*df$coverage_fraction/sum(df$coverage_fraction)) |> sum(),
        ((df$value<20)*df$coverage_fraction/sum(df$coverage_fraction)) |> sum(),
        ((df$value<40)*df$coverage_fraction/sum(df$coverage_fraction)) |> sum(),
        ((df$value<60)*df$coverage_fraction/sum(df$coverage_fraction)) |> sum()
                  ))
    }
  }) )
  simplificacion_interseccion_cobertura_df=do.call(rbind,simplificacion_interseccion_cobertura) |> 
    as.data.frame()
  ##Guardamos las aportaciones de cada AGEB (4 aportaciones)
  ##Deberemos guardar para cada clues, una lista que contiene, id, y cuatro aportaciones
  simplificacion_interseccion_cobertura_df$id=1:nrow(demograficos_scince)
  simplificacion_interseccion_cobertura_df=simplificacion_interseccion_cobertura_df |> dplyr::filter(!dplyr::if_all(V1:V4, is.na))
  lista_aportaciones_por_clues[[length(lista_aportaciones_por_clues)+1]]=simplificacion_interseccion_cobertura_df
}
saveRDS(lista_aportaciones_por_clues,file = "outputs/aportaciones_de_ageb_por_clues.rds")

demograficos_scince_drop_geom=demograficos_scince |> 
  st_drop_geometry() |> dplyr::select(-CVEGEO,-CVE_AGEB,-NOM_MUN,-NOMGEO)
demograficos_scince_drop_geom[demograficos_scince_drop_geom<0]=0
calculo_aportaciones_demograficos=lista_aportaciones_por_clues |> lapply(\(clue_i){#para cada clue, se definen todas las variables demográficas con la suma proporcional
  #clue_i=lista_aportaciones_por_clues[[15]]
  copia_demograficos_clue_i_T10=(diag(clue_i$V1 )%*%(demograficos_scince_drop_geom[clue_i$id,] |> as.matrix()))|> 
    as.data.frame() |> 
    dplyr::summarise_all(\(x){sum(x,na.rm=T)})|> setNames(paste0(colnames(demograficos_scince_drop_geom ),"_T10") )
  copia_demograficos_clue_i_T20=(diag(clue_i$V2 )%*%(demograficos_scince_drop_geom[clue_i$id,] |> as.matrix()))|> 
    as.data.frame() |> 
    dplyr::summarise_all(\(x){sum(x,na.rm=T)})|> setNames(paste0(colnames(demograficos_scince_drop_geom ),"_T20") )
  copia_demograficos_clue_i_T40=(diag(clue_i$V3 )%*%(demograficos_scince_drop_geom[clue_i$id,] |> as.matrix()))|> 
    as.data.frame() |> 
    dplyr::summarise_all(\(x){sum(x,na.rm=T)})|> setNames(paste0(colnames(demograficos_scince_drop_geom ),"_T40") )
  copia_demograficos_clue_i_T60=(diag(clue_i$V4 )%*%(demograficos_scince_drop_geom[clue_i$id,] |> as.matrix()))|> 
    as.data.frame() |> 
    dplyr::summarise_all(\(x){sum(x,na.rm=T)}) |> setNames(paste0(colnames(demograficos_scince_drop_geom ),"_T60") )
  return(list(copia_demograficos_clue_i_T10,
              copia_demograficos_clue_i_T20,
              copia_demograficos_clue_i_T40,
              copia_demograficos_clue_i_T60) |> unlist())
  })
calculo_aportaciones_demograficos_df=do.call(rbind,calculo_aportaciones_demograficos) |> as.data.frame()
calculo_aportaciones_demograficos_df$CLUES=clues_solicitados$CLUES
calculo_aportaciones_demograficos_df=calculo_aportaciones_demograficos_df |> 
  dplyr::relocate(CLUES,.before = POB1_T10)

calculo_aportaciones_demograficos_df |> write.csv("outputs/aportaciones_de_ageb_por_clues.csv",row.names = F)

##Vamos a guardar estos clues más cercanos
cluesN1 |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> dplyr::mutate(tiempo_promedio_CLUES_N2=ifelse((tiempo_promedio_CLUES_N2)>400,400,tiempo_promedio_CLUES_N2) ) |> 
  st_write("outputs/cluesN1_info_accesibilidad.geojson",driver = "GeoJSON",append = F,delete_dsn = T)
