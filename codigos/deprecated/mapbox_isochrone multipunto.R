##Otra alternativas

library(httr)
library(sf)
library(sp)
library(geojsonR)
library(osrm)

bks <-  c(5,15,25,35,45,60,75,90)

# Get isochones with lon/lat coordinates
iso1 <- osrmIsochrone(loc = clues_en_operacion[1,], 
                      breaks = bks,res = 50)
# Get isochones with lon/lat coordinates
iso2 <- osrmIsochrone(loc = clues_en_operacion[25,], 
                      breaks = bks, res = 50)

coords_1=gsub(" ","",gsub("\\)","",gsub("\\(","",gsub("c","",clues_en_operacion[1,] |> st_geometry() |> as.character()))))
coords_2=gsub(" ","",gsub("\\)","",gsub("\\(","",gsub("c","",clues_en_operacion[25,] |> st_geometry() |> as.character()))))
time <- paste(c(5,15,25,35),collapse = ",")
source("codigos/token_mapbox.R")
# Download data from Mapbox API
library(mapgl)
getIsochrones_mapbox=function(coord,times=c(5,15,25,35)){
  iso.url <- paste("https://api.mapbox.com/isochrone/v1/mapbox/driving/",
                   coord,
                   "?contours_minutes=",paste(times,collapse = ","),"&polygons=true&access_token=",Sys.getenv("token_mapbox"),sep = "")
  
  #GET()# Compile each individual catchment area polygon
  r         <- GET(url = iso.url)
  r.header  <- headers(r)
  rr        <- content(r, "text")
  rrr       <- FROM_GeoJson(rr)
  
  #Generalizable a n<=4 tiempos
  sr1       <- Polygon(rrr[["features"]][[1]][["geometry"]][["coordinates"]], hole = TRUE)
  srs1      <- Polygons(list(sr1), times[1])
  sr2       <- Polygon(rrr[["features"]][[2]][["geometry"]][["coordinates"]], hole = TRUE)
  srs2      <- Polygons(list(sr2), times[2])
  sr3       <- Polygon(rrr[["features"]][[3]][["geometry"]][["coordinates"]], hole = TRUE)
  srs3      <- Polygons(list(sr3), times[3])
  sr4       <- Polygon(rrr[["features"]][[4]][["geometry"]][["coordinates"]], hole = TRUE)
  srs4      <- Polygons(list(sr4), times[4])
  
  poly    <- SpatialPolygons(list(srs4,srs3, srs2, srs1), 1:4)
  mapbox  <- SpatialPolygonsDataFrame(poly,data = data.frame(name = c(4,3,2,1),
                                                             row.names = row.names(poly)))
  
  # Add projection
  proj4string(mapbox) <- "+proj=longlat +datum=WGS84 +no_defs"
  
  # Check isochrone polygons
  return(mapbox)
}
clue_1=getIsochrones_mapbox(coord = gsub(" ","",gsub("\\)","",gsub("\\(","",gsub("c","",clues_en_operacion[1,] |> st_geometry() |> as.character()))),
                                         ),times=c(10,20,40,60)
                            )
clue_2=getIsochrones_mapbox(coord = gsub(" ","",gsub("\\)","",gsub("\\(","",gsub("c","",clues_en_operacion[25,] |> st_geometry() |> as.character()))))
                            ,times=c(10,20,40,60))

plot(clue_1, col = "blue", add=T)
plot(clue_2, col='red', add=T)

library(leaflet)

leaflet() |> 
  addTiles() |> 
  addPolygons(data=clue_1 ,fillColor = "blue",
              label = (clue_1@polygons) |> lapply(\(z)return(z@ID)) |> unlist(),
              group = "A") |> ##Se ponen arriba para obtener la union de lo que por definicion es disjunto
  addPolygons(data=clue_2,fillColor = "red",color = "red",
              #label = iso2$isomin,
              group = "B") |> 
  addMarkers(data=clues_en_operacion[1,]) |> 
  addMarkers(data=clues_en_operacion[25,]) 

