library(sf)
library(aws.s3)
source("codigos/SIGEH_isochrone.R")
local=DBI::dbConnect(RSQLite::SQLite(), "clues_demograficos_municipios_simple.sqlite")#Contiene CLUES, Municipios y AGEBS
clues_en_operacion=st_read(local,"clues_en_operacion")
isocronas=function(punto_referencia_fijo,modelo_accesibilidad=T.GC,crs=4326){
  isocronas_niveles_fijos=tryCatch({
    res_raster <- gdistance::accCost(T.GC, punto_referencia_fijo |> st_geometry() |> st_transform(st_crs("EPSG:32614")) |> unlist())
    
    contornos <- raster::rasterToContour(res_raster, levels = 10 * c(1:9)) |> 
      st_as_sf() |> 
      st_set_crs(st_crs("EPSG:32614"))
    contornos|> st_transform(crs)
  },
  error = function(e) {
    message("Error en accCost")
    return(NA)
  })
  
  return(isocronas_niveles_fijos)
}

##Ejemplo
i=sample(1:nrow(clues_en_operacion),size = 1)
punto_referencia_fijo=clues_en_operacion[i,]
isocronas(punto_referencia_fijo) |> raster::plot()

#Para cada clues, se calcula su isocrona y se guarda como geojson

for(i in 1:nrow(clues_en_operacion)){
  row=clues_en_operacion[i,]
  isocronas_i=row |> isocronas()|> 
    dplyr::arrange(dplyr::desc(level)) |> 
    st_transform(st_crs("EPSG:4326"))
  # st_write(isocronas_i,
  #          paste0("outputs/isocronas_pre_calculadas/",row$CLUES,".geojson"),driver='GeoJSON',append = F,delete_dsn  = T )
  aws.s3::put_object(file=paste0("outputs/isocronas_pre_calculadas/",row$CLUES,".geojson"),object = paste0("isocronas/",row$CLUES,".geojson"),
                     bucket = Sys.getenv("bucket"))
}


