icons <- iconList(
  "PRIMER NIVEL"  = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png", iconWidth = 25, iconHeight = 41),
  "SEGUNDO NIVEL" = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png", iconWidth = 25, iconHeight = 41),
  "TERCER NIVEL"  = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", iconWidth = 25, iconHeight = 41)
)
colores_markers <- c(
  "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png"   = "#2A7FFF", # Azul (Primer Nivel)
  "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png" = "#AF7AC5", # Violeta (Segundo Nivel)
  "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png"    = "#E74C3C"  # Rojo (Tercer Nivel)
)
addMarkers_custom = function(proxy, data) {
  if("PRIMER NIVEL"%in% unique(data$NIVEL.ATENCION)){
    funcion_js="function (mapZoom) {
      if (mapZoom > 12) {
        return 0;
      } else {
        return 70;
      }
    }"
  }
  else{funcion_js="function (mapZoom) {
      if (mapZoom > 10) {
        return 0;
      } else {
        return 70;
      }
    }"}
  proxy |> addMarkers(
    data = data,
    # Aquí mapeamos la columna NIVEL.ATENCION con nuestra lista de iconos
    icon = ~icons[NIVEL.ATENCION] ,
    label =paste0(data$CLUES,"-",data$NOMBRE.DE.LA.UNIDAD) ,
    group = "CLUES",clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T,
                                                          maxClusterRadius = 
                                                            htmlwidgets::JS(funcion_js
                                                            )
    ) 
  ) |> 
    addSearchFeatures(targetGroups = "CLUES")

}
