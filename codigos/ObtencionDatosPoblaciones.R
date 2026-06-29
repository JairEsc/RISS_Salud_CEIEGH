rurales = "C:/SCINCE 2020/13_HGO/cartografia/ageb_urb.shp" |>  sf::read_sf() |>  sf::st_drop_geometry()
localidades = "C:/SCINCE 2020/13_HGO/cartografia/loc_urb.shp" |>  sf::st_read(options = "ENCODING=latin1") |>  sf::st_drop_geometry()
localidades_rurales = "C:/SCINCE 2020/13_HGO/cartografia/loc_rur.shp" |>  sf::st_read(options = "ENCODING=latin1") |>  sf::st_drop_geometry()

localidades = localidades |> 
  dplyr::select(-c(NOM_ENT:CABECERA))

localidades_rurales = localidades_rurales |> 
  dplyr::select(-c(NOM_ENT:CVE_AGEB))


datos = localidades |> 
  dplyr::bind_rows(localidades_rurales) |> 
  dplyr::bind_rows(rurales) |> 
  dplyr::arrange(CVEGEO)


datos = datos |> 
  dplyr::mutate(
    dplyr::across(
      .cols = POB1:OID ,
      .fns =  ~ dplyr::if_else(condition = .x < 0, true = 0, false = .x)
    )
  )



descriptor = "C:/SCINCE 2020/13_HGO/descriptores/desc_cpv2020.dbf" |>  sf::st_read(options = "ENCODING=latin1")


# datos_general = datos |>
#   dplyr::select(CVEGEO, POB2, POB4, POB5, POB7, POB28:POB36, POB23, POB20)
# 
# 
# datos_general = datos_general |>
#   dplyr::select(-dplyr::contains("_"))
# 
# 
# 
# datos_general = datos_general |>
#   dplyr::mutate(
#     dplyr::across(
#       .cols = POB2:POB20,
#       .fns = ~ dplyr::if_else(condition = .x < 0, true = 0, false = .x)
#     )
#   )
# 
# 
# datos_general = datos_general |>
#   dplyr::mutate(
#     `POB 20a59_1` = rowSums(dplyr::across(POB29:POB36), na.rm = T),
#     `POB 20a59_2` = POB20 - POB28 - POB23
#   )


datos_general = datos |>
  dplyr::select(CVEGEO, POB2, POB4, POB5, POB7, POB28, POB23, POB20)


datos_general = datos_general |>
  dplyr::mutate(
    `POB_20a59` = POB20 - POB28 - POB23
  )

datos_general = datos_general |> 
  dplyr::select(-POB20) |> 
  dplyr::rename(
    `POB_0a2` = POB2,
    `POB_3a5` = POB4,
    `POB_6a11` = POB5,
    `POB_12a14` = POB7,
    `POB_15a19` = POB28,
    `POB_60ymas` = POB23
  ) |> 
  dplyr::relocate(POB_20a59, .after = POB_15a19)




 ###
datos_femenina = datos |>
  dplyr::select(CVEGEO, POB43, POB45, POB46, POB48, POB70, POB65, POB62)


datos_femenina = datos_femenina |>
  dplyr::mutate(
    `POBF_20a59` = POB62 - POB70 - POB65
  )

datos_femenina = datos_femenina |> 
  dplyr::select(-POB62) |> 
  dplyr::rename(
    `POBF_0a2` = POB43,
    `POBF_3a5` = POB45,
    `POBF_6a11` = POB46,
    `POBF_12a14` = POB48,
    `POBF_15a19` = POB70,
    `POBF_60ymas` = POB65
  ) |> 
  dplyr::relocate(POBF_20a59, .after = POBF_15a19)





####

datos_masculina = datos |>
  dplyr::select(CVEGEO, POB85, POB87, POB88, POB90, POB111, POB106, POB103)


datos_masculina = datos_masculina |>
  dplyr::mutate(
    `POBM_20a59` = POB103 - POB111 - POB106
  )

datos_masculina = datos_masculina |> 
  dplyr::select(-POB103) |> 
  dplyr::rename(
    `POBM_0a2` = POB85,
    `POBM_3a5` = POB87,
    `POBM_6a11` = POB88,
    `POBM_12a14` = POB90,
    `POBM_15a19` = POB111,
    `POBM_60ymas` = POB106
  ) |> 
  dplyr::relocate(POBM_20a59, .after = POBM_15a19)



general = datos_general |> 
  dplyr::left_join(y = datos_masculina, by = "CVEGEO") |> 
  dplyr::left_join(y = datos_femenina, by = "CVEGEO")




general |>  write.csv("outputs/poblaciones_intervalos.csv", fileEncoding = "UTF-8", row.names = F)












