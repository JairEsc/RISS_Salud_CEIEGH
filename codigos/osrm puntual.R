install.packages('osrm')
library(osrm)
iso_1=clues_en_operacion[1,] |> osrmIsochrone(breaks = c(5,10,15,20),res = 70,osrm.profile = "car")




library(leaflet)
leaflet() |> addTiles() |> 
  addPolygons(data=iso_1,label = iso_1$isomin) |> 
  addMarkers(data=clues_en_operacion[1,])

clues_en_operacion[1:5,] |> sf::st_union()
osrmIsochrone(loc = clues_en_operacion[1:5,] |> sf::st_union(),breaks = c(5,10,20))
z$geometry |> plot()



leaflet() |> addTiles() |> 
  addPolylines(
data=osrmTrip(loc =clues_en_operacion[1:30,] ,overview = "full")[[1]]$trip) |> 
  addMarkers(data=clues_en_operacion[1:30,])
