sheets = readxl::excel_sheets("inputs/UNIDADES_SALUD_HGO.xlsx")

datos_lista = lapply(sheets, function(x) {
  readxl::read_excel("inputs/UNIDADES_SALUD_HGO.xlsx", sheet = x)
})

names(datos_lista) = sheets


operacion = dplyr::bind_rows(datos_lista[1:6])

operacion = operacion |> 
  sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), remove = F, crs = 4326, na.fail = F)

operacion |>  sf::write_sf("outputs/operacion.geojson")

fuera = datos_lista[["CLUES_202509 (Fuera_Operación)"]]
fuera = fuera |> 
  dplyr::mutate(LONGITUD = LONGITUD |>  as.numeric(),
                LATITUD = LATITUD |>  as.numeric()) |> 
  sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), remove = F, crs = 4326, na.fail = F)

fuera |>  sf::write_sf("outputs/fuera_de_operacion.geojson")
