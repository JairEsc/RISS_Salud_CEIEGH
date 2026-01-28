## Insumos

Hasta ahora, se establece periodicidad anual o semestral las actualizaciones de: 
    - CLUES (Georeferenciados)
    - Capacidad de Atención 
Por definirse, la actualización de los AGEBs, Localidades Urbanas y Rurales (Esta versión utiliza marco geoestadístico de INEGI, dic. 2020 (se considerò utilizar 2025 pero hay que hacer match entre demográficos 2020 y cartográficos 2025))

## Procesos

    - Construcción única de modelo de accesibilidad carretera (Isocronas SIGEH).
    - Incorporación de API (mapbox) para la consulta de isocronas a niveles fijos (10,20,40,60) de puntos variables (CLUES / agebs). --Se excluye por simplicidad.
    - Ejecución única de códigos: 
      -no_usar_calculos_accesibilidad_agebs.R -> (demograficos_info_accesibilidad.geojson)
      -no_usar_calculos_accesibilidad_clues.R -> (clues_Nk_info_accesibilidad.geojson)
      -no_usar_calculos_rasters_accesibilidad.R -> (acces_CLUES_nk_max90.tif)
 
*pendiente*: Cálculo de número de personas a más de X minutos -> Tab estadísticas
    Resumen. Se toman 3 cartografías del marco geostadístico de INEGI
        -AGEB
        -Localidad Urbana (Tipo polígono)
        -Localidad Rural (Tipo punto)
    Se satisface que cualquier AGEB pertenece a una única localidad Urbana (AGEB es partición de Localidades Urbanas). 
    Se considera un buffer de 150m de las localidades rurales para tomarlas como polígono (En un futuro podrían reemplazarse algunos polígonos por versiones más actuales (por ejemplo marco geostadístico 2025))
    Se aplica una simplificación para disminuir el tamaño (MB) de los polígonos
    La unión de éstas geometrías y datos demográficos (fuente SCINCE) definen la primera base. 

    Para 3 conjuntos de clues seleccionados (Nivel 1, Nivel 2 y Ambos), se calcula el raster de accesibilidad general y se agregan los tiempo promedios de accesibilidad a cada polígono de acuerdo a su intersección con el raster (extract). Esto define el segundo checkpoint (demográficos + accesibilidad promedio a niveles). 

    Con este avance, es trivial determinar el nombre y tiempo promedio del CLUES de nivel 2 más cercano. 
    Nos fijamos en la columna calculada anteiormente para determinar el tiempo mínimo a un hospital y hacemos un join por vecino más cercano (st_join(st_nearest_feature)) para obtener clave de CLUES y nombre de la unidad. Se agregan como columnas y se guarda el progreso.

    Para calcular el número de clues (de nivel N) se considera el subconjunto de pixeles con intersección no vacía al polígono (ageb) y se calcula la distancia de cada uno de los centroides de los pixeles a todos los CLUES seleccionados. (Comentario técnico: De haberse tomado el centroide del polígono, los agebs "grandes" podrían tener información poco precisa. Creemos que el promedio de centroides de pixeles que cubren al polígono es un mejor acercamiento)

    (Comentarío técnico: Como es de esperarse, el costo computacional es alto. Dada la periodicidad de actualización de información, no creo que valga la pena optimizar la actualización de estos datos.
    Utilizamos una librería especializada en extracts para mejorar los tiempos de ejecución)


*pendiente*: Cuántas opciones tiene cada AGEB de cada nivel -> Pre-calculado y mostrado en modal
*pendiente*: Cuántas opciones tiene cada AGEB de cada nivel a qué tiempos. -> Pre-calculado y mostrado en modal
*pendiente*: CLUES nivel 2 más cercano (nombre) y tiempo -> Pre-calculado y mostrado en modal 

*pendiente*: Incorporar capacidades de atención por CLUES.

*pendiente*: Diagrama de tecnologías 
[Diagrama de tecnologías en draw.io](https://drive.google.com/file/d/1JwbSRxKlOHuxOyDfWusGGSZM02ikZuwh/view?usp=sharing)

## Productos

*pendiente*: Regionalización como producto final.
