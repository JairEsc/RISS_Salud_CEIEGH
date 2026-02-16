##Parece que no hay manera sencilla de calcular la distancia al más cercano de los puntos 

library(osrm)
library(sf)

bks <-  c(5,15,25,35,45,60,75,90)

# Get isochones with lon/lat coordinates
iso1 <- osrmIsochrone(loc = clues_en_operacion[1,], 
                      breaks = bks,res = 70)
# Get isochones with lon/lat coordinates
iso2 <- osrmIsochrone(loc = clues_en_operacion[25,], 
                      breaks = bks, res = 70)
plot(iso1$geometry)
plot(iso2$geometry,col='red',add=T)
# intersect isochrones
sf::sf_use_s2(F)
inter <- sf::st_intersection(iso1, iso2)
library(sf)
plot(inter$geometry)
#> although coordinates are longitude/latitude, st_intersection assumes that they are planar
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries
# extrct only polygons
inter <- sf::st_collection_extract(inter, "POLYGON")
inter=inter|>
  dplyr::rowwise() |> 
  dplyr::mutate(
    isomin_f=min(isomin,isomin.1),
    isomax_f=min(isomax,isomax.1),
  ) |> 
  dplyr::ungroup() 

iso <- sf::st_collection_extract(inter, "POLYGON")

# map... O sea que en realidad queremos la union. Podemos explorar la dif. simétrica + interseccion
dif_sim=sf::st_sym_difference(iso1,iso2) 
dif_sim=dif_sim|>
  dplyr::rowwise() |> 
  dplyr::mutate(
    isomin_f=min(isomin,isomin.1),
    isomax_f=min(isomax,isomax.1),
                ) |> 
  dplyr::ungroup() |> 
  st_cast("POLYGON") |> 
  spatialEco::sf_dissolve(y = 'isomin_f',overlaps = T)





library(leaflet)

leaflet() |> 
  addTiles() |> 
  addPolygons(data=iso1,fillColor = "blue",label = iso1$isomin,group = "A") |> ##Se ponen arriba para obtener la union de lo que por definicion es disjunto
  addPolygons(data=iso2,fillColor = "red",color = "red",label = iso2$isomin,group = "B") |> 
  addPolygons(data=iso,label = iso$isomin_f,color = "black",fillColor = "black",group = "inter") |> 
  #addPolygons(data=dif_sim,label = dif_sim$isomin_f,color = "black",fillColor = "black",group = "inter") |> 
  addLayersControl(overlayGroups = c("inter","A","B")) |> 
  addMarkers(data=clues_en_operacion[1,]) |> 
  addMarkers(data=clues_en_operacion[25,]) 
