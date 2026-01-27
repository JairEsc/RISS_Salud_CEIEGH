 #Estadísticas fijas y estáticas. 
demograficos_scince=st_read("outputs/demograficos_cartograficos_scince_20.geojson")
#Ya tenemos calculadas las accesibilidades por nivel a máx 90 minutos. 
demograficos_scince$POB1 |> sum()#3082841

##1.- Num de personas a más de X minutos por nivel de atención. 

##Máximo 90 minutos a cualquier nivel 
##Máximo 90 minutos a nivel 1 
##Máximo 90 minutos a nivel 2


##Isocronas a multipolígonos
clues_solicitados=clues_en_operacion |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
#tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_CLUES_max90.tif")
# leaflet() |> addTiles() |> 
#   addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = "Spectral",opacity = 0.1,group = "Accesibilidad en minutos")
# 
# iso1_sigeh=raster::rasterToContour(tiempo_zona, levels = c(10,20,40,60))|> st_as_sf() |> st_set_crs(st_crs("EPSG:32614")) |>st_transform(st_crs("EPSG:4326"))
# 
# extract_demos=raster::extract(tiempo_zona, demograficos_scince |> st_transform(st_crs("EPSG:32614")),
#                 method = 'simple', buffer = 100, small = T, cellnumbers = FALSE,
#                 fun = mean, na.rm = TRUE)

demograficos_scince$tiempo_promedio_CLUES=extract_demos[,1]
demograficos_scince$tiempo_promedio_CLUES[is.nan(demograficos_scince$tiempo_promedio_CLUES)]=NA
demograficos_scince$tiempo_promedio_CLUES[demograficos_scince$tiempo_promedio_CLUES>60]=NA

leaflet() |>
  addTiles() |>
  addPolygons(data=demograficos_scince |> dplyr::filter(is.na(tiempo_promedio_CLUES) )) |>
  addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = "Spectral",opacity = 0.1,group = "Accesibilidad en minutos")

##Isocronas a multipolígonos
clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="PRIMER NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
#tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_CLUESN1_max90.tif")

iso1_sigeh=raster::rasterToContour(tiempo_zona, levels = c(10,20,40,60))|> st_as_sf() |> st_set_crs(st_crs("EPSG:32614")) |>st_transform(st_crs("EPSG:4326"))



extract_demos=raster::extract(tiempo_zona, demograficos_scince |> st_transform(st_crs("EPSG:32614")),
                method = 'simple', buffer = 100, small = T, cellnumbers = FALSE,
                fun = mean, na.rm = TRUE)

demograficos_scince$tiempo_promedio_CLUES_N1=extract_demos[,1]
demograficos_scince$tiempo_promedio_CLUES_N1[is.nan(demograficos_scince$tiempo_promedio_CLUES_N1)]=NA
demograficos_scince$tiempo_promedio_CLUES_N1[demograficos_scince$tiempo_promedio_CLUES_N1>60]=NA



clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="SEGUNDO NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
#tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_CLUESN1_max90.tif")
iso1_sigeh=raster::rasterToContour(tiempo_zona, levels = c(10,20,40,60))|> st_as_sf() |> st_set_crs(st_crs("EPSG:32614")) |>st_transform(st_crs("EPSG:4326"))

extract_demos=raster::extract(tiempo_zona, demograficos_scince |> st_transform(st_crs("EPSG:32614")),
                method = 'simple', buffer = 100, small = T, cellnumbers = FALSE,
                fun = mean, na.rm = TRUE)

demograficos_scince$tiempo_promedio_CLUES_N2=extract_demos[,1]
demograficos_scince$tiempo_promedio_CLUES_N2[is.nan(demograficos_scince$tiempo_promedio_CLUES_N2)]=NA
demograficos_scince$tiempo_promedio_CLUES_N2[demograficos_scince$tiempo_promedio_CLUES_N2>60]=NA



demograficos_scince |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/demograficos_cartograficos_scince_20_c_accesibilidad.geojson",driver='GeoJSON',append = F,delete_dsn = T)

demograficos_scince=st_read("outputs/demograficos_cartograficos_scince_20_c_accesibilidad.geojson")
## Tiempo categórico a cada tipo de clues
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
##Clave, Nombre de localidad, Nombre de municipio. Pob Total. Pob Afiliada. Piramide poblacional. Distribución de afiliación. 
##Tiempo promedio a CLUES de nivel 1
##Tiempo promedio a CLUES de nivel 2 y nombre de la más cercana
demograficos_scince |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/demograficos_cartograficos_scince_20_c_accesibilidad_c_CLUES2_nearest.geojson",driver='GeoJSON',append=F,delete_dsn = T)

##
demograficos_scince |> st_drop_geometry()|> dplyr::group_by(tiempo_promedio_CLUES_c) |> 
  dplyr::summarise(num_personas=sum(POB1,na.rm=T))
demograficos_scince |> st_drop_geometry()|> dplyr::group_by(tiempo_promedio_CLUES_N2_c) |> 
  dplyr::summarise(num_personas=sum(POB1,na.rm=T))

















###############
###

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
for(cve_unica in unique(resultado$CVEGEO)){
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
    conteo_clues_N2_por_ageb[nrow(conteo_clues_N2_por_ageb)+1,]=c(
      cve_unica,num_clues_a_menos_de_10,num_clues_a_menos_de_20,num_clues_a_menos_de_40,
      num_clues_a_menos_de_60
    )
  }

demograficos_scince=demograficos_scince |> merge(conteo_clues_N2_por_ageb,by='CVEGEO',all.x=T)
demograficos_scince |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/demograficos_cartograficos_scince_20_c_accesibilidad_c_CLUES2_nearest_c_num_CLUESN2.geojson",driver='GeoJSON',append=F,delete_dsn = T)
###############
###
clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="PRIMER NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
conteo_clues_N1_por_ageb=data.frame() |> dplyr::mutate(CVEGEO=NA,
                              CLUES_N1_10=NA,
                              CLUES_N1_20=NA,
                              CLUES_N1_40=NA,CLUES_N1_60=NA
                              )
for(cve_unica in unique(resultado$CVEGEO)[1:2000]){
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

demograficos_scince=demograficos_scince |> merge(conteo_clues_N2_por_ageb,by='CVEGEO',all.x=T)
demograficos_scince |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  st_write("outputs/demograficos_cartograficos_scince_20_c_accesibilidad_c_CLUES2_nearest_c_num_CLUESN2.geojson",driver='GeoJSON',append=F,delete_dsn = T)
