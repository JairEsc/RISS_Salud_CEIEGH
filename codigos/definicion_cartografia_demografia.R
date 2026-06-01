demograficos_scince=sf::st_read("outputs/demograficos_con_info_accesibilidad.geojson")
##único vectorial con cartografia
##
##CVEGEO
##POBs
##SALUDs
##Nombre municipio
##Nombre de Localidad
##Tiempos promedios a CLUES
##Tiempos promedios a CLUES N1
##Tiempos promedios a CLUES N2
##Categóricos
#CLUES más cercano
#Nombre CLUES más cercano
#Num CLUES N1 a menos de 10 minutos
#Num CLUES N1 a menos de 20 minutos
#Num CLUES N1 a menos de 30 minutos
#Num CLUES N1 a menos de 40 minutos
#Num CLUES N1 a menos de 60 minutos

##

# 
# poblacion = "outputs/poblaciones_intervalos.csv" |>  read.csv()
# 
# 
# 
# demograficos_scince = demograficos_scince |>
#   dplyr::left_join(y = poblacion, by = "CVEGEO")
# 
# demograficos_scince = demograficos_scince |> 
#   dplyr::relocate(POB_0a2:POBF_60ymas, .after = POB84)
# 
# 
# demograficos_scince |>  sf::write_sf("outputs/demograficos_con_info_accesibilidad1.geojson", overwrite = TRUE)


