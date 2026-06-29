source("codigos/csv_to_geojson.R")
source("codigos/definicion_cartografia_demografia.R")

con <- DBI::dbConnect(RSQLite::SQLite(), "clues_demograficos_municipios_simple.sqlite")
##Falta agregar datos calculados a los geojsons
clues_en_operacion_s="outputs/clues_en_operacion_con_info_accesibilidad.geojson" |> st_read()
clues_en_operacion_s=clues_en_operacion_s |> 
  dplyr::select(CLUES,MUNICIPIO,LOCALIDAD,NOMBRE.DE.LA.UNIDAD,NOMBRE.DE.LA.INSTITUCION,NIVEL.ATENCION,Conteo_N1_T10:SALUD10_T60,geometry)
clues_en_operacion_s$CLUES |> nchar() |> unique()#CHAR(size)
clues_en_operacion_s |> dplyr::select(Conteo_N1_T10:Conteo_N3_T60) |> st_drop_geometry() |> lapply(\(x){as.numeric(x)} )|> unlist() |> max()#SMALLINT
clues_en_operacion_s |> dplyr::select(POB1_T10:SALUD10_T60) |> st_drop_geometry()|> lapply(\(x){as.numeric(x)} )|> unlist() |> max()#MEDIUMINT

clues_en_operacion_s |> dplyr::select(CLUES_N1_mas_cercano,CLUES_N2_mas_cercano,CLUES_N3_mas_cercano) |> 
  st_drop_geometry() |> lapply(nchar) |> unlist() |> unique() ##CHAR(size)
clues_en_operacion_s |> dplyr::select(Tiempo_promedio_CLUES_N1_mas_cercano,Tiempo_promedio_CLUES_N2_mas_cercano,Tiempo_promedio_CLUES_N3_mas_cercano) |> 
  st_drop_geometry() |> lapply(nchar) |> unlist() |> unique() ##FLOAT(p)

clues_en_operacion |> colnames()

tipos_columnas <- c(
  CLUES = "CHAR(11)",
  MUNICIPIO = "TEXT",
  LOCALIDAD = "TEXT",
  NOMBRE.DE.LA.UNIDAD = "TEXT",
  NIVEL.ATENCION = "TEXT",
  CLUES_N1_mas_cercano = "CHAR(11)",
  CLUES_N2_mas_cercano = "CHAR(11)",
  CLUES_N3_mas_cercano = "CHAR(11)",
  Tiempo_promedio_CLUES_N1_mas_cercano = "FLOAT(3)",
  Tiempo_promedio_CLUES_N2_mas_cercano = "FLOAT(3)",
  Tiempo_promedio_CLUES_N3_mas_cercano = "FLOAT(3)"
)
 
columnas_conteo <- grep("^Conteo_", names(clues_en_operacion_s), value = TRUE)
tipos_conteo <- setNames(rep("SMALLINT", length(columnas_conteo)), columnas_conteo)

columnas_pob_salud <- grep("^(POB|SALUD)", names(clues_en_operacion_s), value = TRUE)
tipos_pob_salud <- setNames(rep("MEDIUMINT", length(columnas_pob_salud)), columnas_pob_salud)

field_types_completo <- c(tipos_columnas, tipos_conteo, tipos_pob_salud)

st_write(
  obj = clues_en_operacion_s, 
  dsn = con, 
  layer = "clues_en_operacion", 
  delete_layer = TRUE, # Usar TRUE si quieres sobrescribir/actualizar la tabla limpia
  field_types = field_types_completo
)

# DBI::dbDisconnect(con)
# con <- DBI::dbConnect(RSQLite::SQLite(), "clues_demograficos_municipios_simple2.sqlite")

#st_write(clues_en_operacion_s, con, "clues_en_operacion", delete_layer = FALSE)

limites_municipales=limites_municipales |> 
  dplyr::select(CVE_MUN,NOM_MUN,geometry) 

st_write(
  obj = limites_municipales|> 
    st_simplify(preserveTopology = T,dTolerance = 100), 
  dsn = con, 
  layer = "limite_municipal", 
  delete_layer = TRUE, # Sobrescribe si ya existía una versión previa sin tipos definidos
  field_types = c(
    CVE_MUN = "CHAR(3)",
    NOM_MUN = "TEXT"
  )
)
# DBI::dbDisconnect(con)
# con <- DBI::dbConnect(RSQLite::SQLite(), "clues_demograficos_municipios_simple3.sqlite")

demograficos_scince |> 
  dplyr::select(POB1:SALUD10)
demograficos_scince |> 
  dplyr::select(CLUES_N1_10:CLUES_N3_60)
demograficos_scince |> 
  dplyr::select(id_clues_N1_mas_cercano,id_clues_N2_mas_cercano,id_clues_N3_mas_cercano)
demograficos_scince |> 
  dplyr::select(tiempo_promedio_clues_N1_mas_cercano,tiempo_promedio_clues_N2_mas_cercano,
                tiempo_promedio_clues_N3_mas_cercano)
demograficos_scince$CVEGEO |> nchar() |> unique() #Puede ser 9 o 13
#demograficos_scince #Los demas son TEXT


demograficos_scince_s <- demograficos_scince |> 
  dplyr::select(
    CVEGEO,CVE_AGEB,NOM_MUN,NOMGEO,POB1:SALUD10,CLUES_N1_10:CLUES_N1_60,CLUES_N2_10:CLUES_N2_60,CLUES_N3_10:CLUES_N3_60,
    id_clues_N1_mas_cercano, id_clues_N2_mas_cercano, id_clues_N3_mas_cercano,
    tiempo_promedio_clues_N1_mas_cercano, tiempo_promedio_clues_N2_mas_cercano, tiempo_promedio_clues_N3_mas_cercano,
    nombre_clues_N1_mas_cercano,nombre_clues_N2_mas_cercano,nombre_clues_N3_mas_cercano
  )

# 3. Definición base de tipos de datos fijos
tipos_demograficos <- c(
  CVEGEO = "VARCHAR(13)",
  id_clues_N1_mas_cercano = "CHAR(11)",
  id_clues_N2_mas_cercano = "CHAR(11)",
  id_clues_N3_mas_cercano = "CHAR(11)",
  tiempo_promedio_clues_N1_mas_cercano = "FLOAT(3)",
  tiempo_promedio_clues_N2_mas_cercano = "FLOAT(3)",
  tiempo_promedio_clues_N3_mas_cercano = "FLOAT(3)"
)

# 4. Reutilización y detección masiva de columnas con grep

# Columnas de Población y Salud (POB1 a SALUD10) -> MEDIUMINT
columnas_pob_salud_scince <- grep("^(POB|SALUD)", names(demograficos_scince_s), value = TRUE)
tipos_pob_salud_scince <- setNames(rep("MEDIUMINT", length(columnas_pob_salud_scince)), columnas_pob_salud_scince)


# Columnas de códigos CLUES por rangos de tiempo (CLUES_N1_10 a CLUES_N3_60) -> CHAR(11)
columnas_clues_tiempo <- grep("^CLUES_N[1-3]_[0-9]+", names(demograficos_scince_s), value = TRUE)
tipos_clues_tiempo <- setNames(rep("CHAR(11)", length(columnas_clues_tiempo)), columnas_clues_tiempo)

# Unimos todas las definiciones de tipos en un solo vector
field_types_scince <- c(tipos_demograficos, tipos_pob_salud_scince, tipos_clues_tiempo)

# 5. Escritura en SQLite aplicando la optimización
st_write(
  obj = demograficos_scince_s |> 
    st_simplify(preserveTopology = T,dTolerance = 100), 
  dsn = con, 
  layer = "demograficos_scince", 
  delete_layer = TRUE, 
  field_types = field_types_scince
)



#st_write(demograficos_scince |> st_transform(4326), con, "demograficos_scince", delete_layer = FALSE)

DBI::dbDisconnect(con)


