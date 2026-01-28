demograficos_scince =st_read("outputs/demograficos_cartograficos_scince_20_c_accesibilidad_c_CLUES2_nearest.geojson")

clues_en_operacion=dplyr::tbl(local,"clues_en_operacion")
clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="PRIMER NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry)) |> st_as_sf()
clues_solicitados=clues_solicitados |> st_intersection(limites_municipales |> dplyr::select(geometry) |> st_transform(4326)) 

tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=200
tiempo_zona[ (tiempo_zona)>200]=200
#tiempo_zona[ tiempo_zona>=90]=NA

puntos_raster <- terra::as.points(terra::rast(tiempo_zona), values = TRUE, na.rm = F)
puntos_sf <- st_as_sf(puntos_raster)
demograficos_scince$CVEGEO |> unique() |> length()
resultado <- st_join(puntos_sf, demograficos_scince |> st_transform(st_crs("EPSG:32614")) |> st_buffer(100), left = FALSE)
resultado$CVEGEO |> unique() |> length()
conteo_clues_N2_por_ageb=data.frame() |> dplyr::mutate(CVEGEO=NA,
                                                       CLUES_N2_10=NA,
                                                       CLUES_N2_20=NA,
                                                       CLUES_N2_40=NA,CLUES_N2_60=NA
)

conteo_clues_N1_por_ageb=data.frame() |> dplyr::mutate(CVEGEO=NA,
                                                       CLUES_N1_10=NA,
                                                       CLUES_N1_20=NA,
                                                       CLUES_N1_40=NA,CLUES_N1_60=NA
)
counter=0
for(cve_unica in unique(resultado$CVEGEO)){
  counter=counter+1
  if(counter%%100==0) print(cve_unica)
  #Tiempo de ejecución esperado: 3 hora
  #cve_unica='130460178'
  puntos_de_ageb=resultado |> dplyr::filter(CVEGEO==cve_unica)
  matriz_de_costos=gdistance::costDistance(T.GC,fromCoords = 
                                             matrix(unlist(puntos_de_ageb$geometry ),nrow = nrow(puntos_de_ageb),ncol = 2,byrow = T)
                                           ,toCoords = 
                                             matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T)
  )
  num_clues_a_menos_de_10=rowSums(matriz_de_costos<10) |> mean() |> round(0)
  num_clues_a_menos_de_20=rowSums(matriz_de_costos<20) |> mean() |> round(0)
  num_clues_a_menos_de_40=rowSums(matriz_de_costos<40) |> mean() |> round(0)
  num_clues_a_menos_de_60=rowSums(matriz_de_costos<60) |> mean() |> round(0)
  conteo_clues_N1_por_ageb[nrow(conteo_clues_N1_por_ageb)+1,]=c(
    cve_unica,num_clues_a_menos_de_10,num_clues_a_menos_de_20,num_clues_a_menos_de_40,
    num_clues_a_menos_de_60
  )
}

demograficos_scince=demograficos_scince |> merge(conteo_clues_N1_por_ageb,by='CVEGEO',all.x=T)
demograficos_scince |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/demograficos_info_accesibilidad_clues",driver='GeoJSON',append=F,delete_dsn = T)

demograficos_c_opciones_CLUES=st_read("outputs/demograficos_cartograficos_scince_20_c_accesibilidad_c_CLUES2_nearest_c_num_CLUESN2_N1.geojson")
