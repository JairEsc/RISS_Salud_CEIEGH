library(httr)
library(sf)
library(sp)
library(geojsonR)
library(osrm)

source("codigos/csv_to_geojson.R")
source("codigos/funciones.R")
source("codigos/token_mapbox.R")


#clues_en_operacion[5,] |> sf::st_coordinates() |> as.data.frame() |> (\(d) paste0(d$X, " , ", d$Y))()

clue_1 = getIsochrones_mapbox(coord = gsub(" ","",gsub("\\)","",gsub("\\(","",gsub("c","",clues_en_operacion[1,] |> st_geometry() |> as.character()))),),
                              times=c(15, 30, 45, 60))

clue_2 = getIsochrones_mapbox(coord = gsub(" ","",gsub("\\)","",gsub("\\(","",gsub("c","",clues_en_operacion[37,] |> st_geometry() |> as.character())))),
                              times=c(15, 30, 45, 60))

clue_3 = getIsochrones_mapbox(coord = gsub(" ","",gsub("\\)","",gsub("\\(","",gsub("c","",clues_en_operacion[1000,] |> st_geometry() |> as.character()))),),
                              times=c(15, 30, 45, 60))


inter1 = interseccion_optima(datos1 = clue_1, datos2 = clue_2)
inter2 = interseccion_optima(datos1 = inter1, datos2 = clue_3)



leaflet() |>  addTiles() |> 
  addPolygons(data = inter2, label = inter2$tiempo, color = paleta(inter2$tiempo)) |>  
  addMarkers(data=clues_en_operacion[1,]) |> 
  addMarkers(data=clues_en_operacion[37,])  |> 
  addMarkers(data =clues_en_operacion[1000,] )


paleta = colorNumeric(palette = c("green", "yellow", "orange", "red"),
             domain = inter2$tiempo)






