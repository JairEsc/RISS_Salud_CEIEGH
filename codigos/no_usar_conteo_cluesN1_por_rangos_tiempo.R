library(sf)
library(terra)
T.GC="inputs/accesibilidad_SIGEH/accesibilidad_carretera.rds" |> readRDS()

clues_solicitados=clues_en_operacion |> #dplyr::filter(NIVEL.ATENCION=="PRIMER NIVEL") |> 
  dplyr::select(NIVEL.ATENCION,geometry) |> dplyr::arrange(NIVEL.ATENCION) |> dplyr::collect() |> 
  dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()

tiempo_zona=terra::rast("inputs/rasters/acces_CLUES_max90.tif")
crs(tiempo_zona)=st_crs("EPSG:32614")$wkt

puntos_raster <- terra::as.points(terra::rast(tiempo_zona), values = TRUE, na.rm = F)
puntos_sf <- st_as_sf(puntos_raster)
demograficos_scince$CVEGEO |> unique() |> length()
resultado <- st_join(puntos_sf, demograficos_scince |> 
                       st_transform(st_crs("EPSG:32614")) |> 
                       st_buffer(100), left = FALSE)
resultado$CVEGEO |> unique() |> length()
#plot(tiempo_zona)

conteo_clues_por_ageb=data.frame() |> dplyr::mutate(CVEGEO=NA,
                                                       CLUES_N1_10=NA,
                                                       CLUES_N1_20=NA,
                                                       CLUES_N1_40=NA,CLUES_N1_60=NA,tiempo_promedio_clues_N1_mas_cercano=NA,id_clues_N1_mas_cercano=NA,
                                                       CLUES_N2_10=NA,
                                                       CLUES_N2_20=NA,
                                                       CLUES_N2_40=NA,CLUES_N2_60=NA,tiempo_promedio_clues_N2_mas_cercano=NA,id_clues_N2_mas_cercano=NA,
                                                       CLUES_N3_10=NA,
                                                       CLUES_N3_20=NA,
                                                       CLUES_N3_40=NA,CLUES_N3_60=NA,tiempo_promedio_clues_N3_mas_cercano=NA,id_clues_N3_mas_cercano=NA,

)

counter=0
num_clues_por_tipo=clues_solicitados |> st_drop_geometry() |> dplyr::group_by(NIVEL.ATENCION) |> dplyr::summarise(conteo=dplyr::n())
num_clues_por_tipo$conteo
estandarizar_matriz_costos=function(matriz_costos){
  ##Como estamos calculando distancias indiscriminadamente (punto_ageb vs. todos los CLUES), algúnos están absurdamente lejos, 
  #y el modelo regresa "Inf"
  max_por_renglon <- apply(matriz_costos, 1, function(x) max(max(x[is.finite(x)], na.rm = TRUE),-1) )
  max_por_renglon[max_por_renglon<=0]=NA
  ind_inf <- which(is.infinite(matriz_costos), arr.ind = TRUE)
  matriz_costos[ind_inf] <- max_por_renglon[ind_inf[, 1]]
  return(matriz_costos)
}
test_ageb=function(cve){
  leaflet::leaflet() |> 
    leaflet::addTiles() |> 
    leaflet::addPolygons(data=demograficos_scince[demograficos_scince$CVEGEO==cve_unica,]) 
}
#detallitos
#846
for(cve_unica in unique(resultado$CVEGEO)){
  counter=counter+1
  if(counter%%1000==0) print(cve_unica)
  #Tiempo de ejecución esperado: 3 horas
  #cve_unica=unique(resultado$CVEGEO)[846]#'130460178'
  puntos_de_ageb=resultado |> dplyr::filter(CVEGEO==cve_unica) 
  matriz_de_costos=gdistance::costDistance(T.GC,fromCoords = 
                                             matrix(unlist(puntos_de_ageb$geometry ),nrow = nrow(puntos_de_ageb),ncol = 2,byrow = T)
                                           ,toCoords = 
                                             matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T)
  )##Lamentablemente nuestro modelo de accesibilidad regresa Infinitos en las zonas donde la fricción es máxima (pendiente cercana a 90), por ejemplo montañas
  if(dim(matriz_de_costos)[1]==1){
    matriz_de_costos=rbind(matriz_de_costos[1,],matriz_de_costos[1,])
  }
  particion_N1_costos=matriz_de_costos[,1:(num_clues_por_tipo$conteo[1])] |> estandarizar_matriz_costos()
  particion_N2_costos=matriz_de_costos[,(1+(num_clues_por_tipo$conteo[1])):(num_clues_por_tipo$conteo[1]+(num_clues_por_tipo$conteo[2]))] |> estandarizar_matriz_costos()
  particion_N3_costos=matriz_de_costos[,(num_clues_por_tipo$conteo[1]+(num_clues_por_tipo$conteo[2])+1):nrow(clues_solicitados)] |> estandarizar_matriz_costos()
  
  num_clues_N1=c(10,20,40,60,NA) |> lapply(\(x){
    ifelse(x |> is.na(),{
           means_tiempo_clues=colMeans(particion_N1_costos,na.rm = T)
           return( c(min(means_tiempo_clues),which.min(means_tiempo_clues)))}
           ,rowSums(particion_N1_costos<x) |> mean(na.rm=T) |> round(0))
    
  }) |> unlist()
  num_clues_N2=c(10,20,40,60,NA) |> lapply(\(x){
    ifelse(x |> is.na(),{
      means_tiempo_clues=colMeans(particion_N2_costos,na.rm = T)
      return( c(min(means_tiempo_clues),which.min(means_tiempo_clues)))}
      ,rowSums(particion_N2_costos<x) |> mean(na.rm=T) |> round(0))
    
  }) |> unlist()
  num_clues_N3=c(10,20,40,60,NA) |> lapply(\(x){
    ifelse(x |> is.na(),{
      means_tiempo_clues=colMeans(particion_N3_costos,na.rm = T)
      return( c(min(means_tiempo_clues),which.min(means_tiempo_clues)))}
      ,rowSums(particion_N3_costos<x) |> mean(na.rm=T) |> round(0))
    
  }) |> unlist()
  
  conteo_clues_por_ageb[nrow(conteo_clues_por_ageb)+1,]=c(
    cve_unica,num_clues_N1,
    num_clues_N2,
    num_clues_N3
  )
}


demograficos_scince=demograficos_scince |> merge(conteo_clues_por_ageb,by='CVEGEO',all.x=T)
demograficos_scince |> dplyr::relocate(geometry,.after = dplyr::last_col()) |> 
  sf::st_write("outputs/demograficos_info_accesibilidad_clues.geojson",driver='GeoJSON',append=F,delete_dsn = T)

