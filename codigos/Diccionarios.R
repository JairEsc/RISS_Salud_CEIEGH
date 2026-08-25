municipal = "../../../../Nueva carpeta/municipios_2026-08-25.xlsx" |> 
  readxl::read_excel()


diccionario_municipal = data.frame(
  Campo = municipal |>  names()
)



diccionario_municipal = diccionario_municipal |> 
  dplyr::mutate(
    Descripcion = dplyr::case_when(
      Campo == "NOM_MUN" ~ "Nombre del municipio",
      Campo == "POB1" ~ "Población total",
      Campo == "SALUD1" ~ "Población afiliada a servicios de salud",
      Campo == "tiempo_promedio_CLUES_N1" ~ "Tiempo promedio a la CLUES de nivel 1 más cercana",
      Campo == "tiempo_promedio_CLUES_N2" ~ "Tiempo promedio a la CLUES de nivel 2 más cercana",
      Campo == "tiempo_promedio_CLUES_N3" ~ "Tiempo promedio a la CLUES de nivel 3 más cercana",
      T ~ Campo
    ) |>  stringr::str_squish()
  )











##############

localidades = "../../../../Nueva carpeta/localidades_2026-08-25.xlsx" |> 
  readxl::read_excel()

diccionario_localidad = data.frame(
  Campo = localidades |>  names()
)


diccionario_localidad = diccionario_localidad |> 
  dplyr::mutate(
    Descripcion = Campo |> 
      gsub(pattern = "POBM", replacement = "Población total masculina de") |> 
      gsub(pattern = "POBF", replacement = "Población total femenina de") |> 
      gsub(pattern = "POB", replacement = "Población total de")  |> 
      
      gsub(pattern = "0a2", replacement = "0 a 2 años") |> 
      gsub(pattern = "3a5", replacement = "3 a 5 años") |> 
      gsub(pattern = "6a11", replacement = "6 a 11 años") |> 
      gsub(pattern = "12a14", replacement = "12 a 14 años") |> 
      gsub(pattern = "15a19", replacement = "15 a 19 años") |> 
      gsub(pattern = "20a59", replacement = "20 a 59 años") |> 
      gsub(pattern = "60ymas", replacement = "60 años y más") |> 
      gsub(pattern = "_", replacement = " ") |>  
      stringr::str_squish()
  ) 



diccionario_localidad = diccionario_localidad |> 
  dplyr::mutate(
    Descripcion = dplyr::case_when(
      Campo == "NOM_MUN" ~ "Nombre del municipio",
      Campo == "POB1" ~ "Población total",
      Campo == "SALUD1" ~ "Población afiliada a servicios de salud",
      Campo == "tiempo_promedio_CLUES_N1" ~ "Tiempo promedio a la CLUES de nivel 1 más cercana",
      Campo == "tiempo_promedio_CLUES_N2" ~ "Tiempo promedio a la CLUES de nivel 2 más cercana",
      Campo == "tiempo_promedio_CLUES_N3" ~ "Tiempo promedio a la CLUES de nivel 3 más cercana",
      Campo == "NOMGEO" ~ "Nombre de la localidad",
      T ~ Descripcion
    )
  )







######

ageb = "../../../../Nueva carpeta/agebs_2026-08-25.xlsx" |> 
  readxl::read_excel()

diccionario_ageb = data.frame(
  Campo = ageb |>  names()
)



diccionario_ageb = diccionario_ageb |> 
  dplyr::left_join(y = diccionario_localidad,
                   by = "Campo"
                     )



diccionario_ageb = diccionario_ageb |> 
  dplyr::mutate(
    Descripcion = dplyr::case_when(
      Campo == "CVEGEO" ~ "Clave geoestadística",
      Campo == "POB42" ~ "Población Femenina",
      Campo == "POB84" ~ "Población Masculina",
      Campo == "CLUES_N1_10" ~ "Número de CLUES de primer nivel a menos de 10 minutos",
      Campo == "CLUES_N1_20" ~ "Número de CLUES de primer nivel a menos de 20 minutos",
      Campo == "CLUES_N1_40" ~ "Número de CLUES de primer nivel a menos de 40 minutos",
      Campo == "CLUES_N1_60" ~ "Número de CLUES de primer nivel a menos de 60 minutos",
      Campo == "tiempo_promedio_clues_N1_mas_cercano" ~ "Tiempo promedio a la CLUES de nivel 1 más cercana",
      Campo == "id_clues_N1_mas_cercano" ~ "Identificador del CLUES de primer nivel más cercano",
      Campo == "nombre_clues_N1_mas_cercano" ~ "Nombre del CLUES de primer nivel más cercano",
      Campo == "CLUES_N2_10" ~ "Número de CLUES de segundo nivel a menos de 10 minutos",
      Campo == "CLUES_N2_20" ~ "Número de CLUES de segundo nivel a menos de 20 minutos",
      Campo == "CLUES_N2_40" ~ "Número de CLUES de segundo nivel a menos de 40 minutos",
      Campo == "CLUES_N2_60" ~ "Número de CLUES de segundo nivel a menos de 60 minutos",
      Campo == "tiempo_promedio_clues_N2_mas_cercano" ~ "Tiempo promedio a la CLUES de nivel 2 más cercana",
      Campo == "id_clues_N2_mas_cercano" ~ "Identificador del CLUES de segundo nivel más cercano",
      Campo == "nombre_clues_N2_mas_cercano" ~ "Nombre del CLUES de primer nivel más cercano",
      Campo == "CLUES_N3_10" ~ "Número de CLUES de tercer nivel a menos de 10 minutos",
      Campo == "CLUES_N3_20" ~ "Número de CLUES de tercer nivel a menos de 20 minutos",
      Campo == "CLUES_N3_40" ~ "Número de CLUES de tercer nivel a menos de 40 minutos",
      Campo == "CLUES_N3_60" ~ "Número de CLUES de tercer nivel a menos de 60 minutos",
      Campo == "tiempo_promedio_clues_N3_mas_cercano" ~ "Tiempo promedio a la CLUES de nivel 3 más cercana",
      Campo == "id_clues_N3_mas_cercano" ~ "Identificador del CLUES de tercer nivel más cercano",
      Campo == "nombre_clues_N3_mas_cercano" ~ "Nombre del CLUES de primer nivel más cercano",
      Campo == "POB_rel" ~ "Porcentaje de la población respecto al total",
      T ~ Descripcion
    )
  )





diccionario_municipal |>  openxlsx::write.xlsx("../../../../Nueva carpeta/diccionario_municipal.xlsx")
diccionario_localidad |>  openxlsx::write.xlsx("../../../../Nueva carpeta/diccionario_localidad.xlsx")
diccionario_ageb |>  openxlsx::write.xlsx("../../../../Nueva carpeta/diccionario_ageb.xlsx")
