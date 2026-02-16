# clues_en_operacion_s=clues_en_operacion |> dplyr::collect() |> 
#   dplyr::mutate(geometry=st_as_sfc(geometry))
nombres_unidades_clues=clues_en_operacion_s |> dplyr::select(CLUES,NOMBRE.DE.LA.UNIDAD) |> st_drop_geometry()
cluesN1_info=st_read("outputs/cluesN1_info_accesibilidad.geojson") 
cluesN1_info=cluesN1_info|> dplyr::left_join(y = nombres_unidades_clues,by=dplyr::join_by(CLUESN2_mas_cercana == CLUES)) |> 
  dplyr::rename(NOMBRE.DE.LA.UNIDAD.CN2.mas_cercana=NOMBRE.DE.LA.UNIDAD) |> 
  dplyr::relocate(NOMBRE.DE.LA.UNIDAD.CN2.mas_cercana,.after = CLUESN2_mas_cercana)
cluesN2_info=st_read("outputs/cluesN2_info_accesibilidad.geojson")
cluesN2_info=cluesN2_info|> dplyr::left_join(y = nombres_unidades_clues,by=dplyr::join_by(CLUESN2_mas_cercana == CLUES)) |> 
  dplyr::rename(NOMBRE.DE.LA.UNIDAD.CN2.mas_cercana=NOMBRE.DE.LA.UNIDAD) |> 
  dplyr::relocate(NOMBRE.DE.LA.UNIDAD.CN2.mas_cercana,.after = CLUESN2_mas_cercana)
cluesN_info=rbind(cluesN1_info,cluesN2_info) 
clues_en_operacion_s=clues_en_operacion_s |> 
  merge(cluesN_info |> st_drop_geometry(),by='CLUES',all.x=T) |> 
  dplyr::relocate(NOMBRE.DE.LA.UNIDAD.CN2.mas_cercana,.after = CLUESN2_mas_cercana)
#st_write(clues_en_operacion_s,dsn = local,layer = "clues_en_operacion",append = F,delete_dsn = T)

#st_read(local,'clues_en_operacion')
#dplyr::tbl(local,"clues_en_operacion") |> dplyr::select(CLUES,geometry) |> dplyr::collect() |> 
#  dplyr::mutate(geometry=st_as_sfc(geometry)) |> st_as_sf()
