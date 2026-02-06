#Cálculos fijos de accesibilidad. 

#Tiempos máximos de 90 minutos para visualización. 

clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="PRIMER NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_CLUESN1_max90.tif")
#Tiempos máximos de 90 minutos para visualización. 

clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="SEGUNDO NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_CLUESN2_max90.tif")

clues_solicitados=clues_en_operacion |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_CLUES_max90.tif",overwrite=T)

clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="TERCER NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry=st_as_sfc(geometry))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_CLUESN3_max90.tif",overwrite=T)

#######Cambiamos modelo de accesibilidad:

T.GC="../../Repositorios/Accesibilidad_a_puntos/Output/T.GC_caminando.rds" |> readRDS()

clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="PRIMER NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
plot(tiempo_zona)
tiempo_zona |> writeRaster("inputs/rasters/acces_peatonal_CLUESN1_max90.tif")
#Tiempos máximos de 90 minutos para visualización. 

clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="SEGUNDO NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_peatonal_CLUESN2_max90.tif")

clues_solicitados=clues_en_operacion |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_peatonal_CLUES_max90.tif",overwrite=T)

clues_solicitados=clues_en_operacion |> dplyr::filter(NIVEL.ATENCION=="TERCER NIVEL") |> dplyr::select(geometry) |> dplyr::collect() |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T))
tiempo_zona=accCost(T.GC, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
tiempo_zona[ is.infinite(tiempo_zona)]=100
tiempo_zona[ tiempo_zona>=90]=NA
tiempo_zona |> writeRaster("inputs/rasters/acces_peatonal_CLUESN3_max90.tif",overwrite=T)
