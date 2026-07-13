#Cálculos fijos de accesibilidad. 
T.GC="inputs/accesibilidad_SIGEH/accesibilidad_carretera.rds" |> readRDS()
T.GC.peatonal="inputs/accesibilidad_SIGEH/accesibilidad_peatonal.rds" |> readRDS()

#Tiempos máximos de 90 minutos para visualización. 
matrices.Transicion=c(T.GC,T.GC.peatonal)
niveles_atencion=c("",clues_en_operacion |> dplyr::pull(NIVEL.ATENCION)|> unique())
sectores=c("",clues_en_operacion |> dplyr::pull(archivo_origen)|> unique())
for(nivel in niveles_atencion){
  for(sector in sectores){
    for(transicion in matrices.Transicion){
      clues_solicitados=clues_en_operacion |> dplyr::filter((NIVEL.ATENCION==nivel | nivel=='') & (archivo_origen==sector | sector=='') ) |> dplyr::select(geometry) |> dplyr::collect() |> 
        dplyr::mutate(geometry= sf::st_as_sfc(structure(geometry,class = "WKB" ),EWKB=T)) |> st_as_sf()
      print(paste0(nrow(clues_solicitados)," CLUES ", nivel, " ",sector))
      if(nrow(clues_solicitados)>0){
        tiempo_zona=gdistance::accCost(transicion, matrix(unlist(clues_solicitados$geometry |> st_transform(32614)),nrow = nrow(clues_solicitados),ncol = 2,byrow = T))
        crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
        tiempo_zona[ is.infinite(tiempo_zona)]=100
        tiempo_zona[ tiempo_zona>=90]=NA
        peat=''
        if(identical(transicion,T.GC.peatonal) ){
          peat='_peaton'
        }
        tiempo_zona |> writeRaster(paste0("inputs/rasters/acces_",
                                          gsub(pattern = " ",replacement = "_",nivel),"_",
                                          gsub(pattern = "ú","u",x = sector),peat,".tif"
                                          ),overwrite=T)
      }
    }
  }
}


