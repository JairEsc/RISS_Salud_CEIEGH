#Exploración de accesibilidad a 1 punto. 
library(sf)
library(httr)
library(sp)
library(geojsonR)
library(osrm)

punto_referencia_fijo=st_point(c(-99.06792,20.54136)) |> st_sfc(crs = 4326)

#Isocronas predefinidas
T1=c(10,20,40,60)
T2=c(50,100,150,200)

#Calculo de isocronas
  #osm
iso1_osm=punto_referencia_fijo |> osrmIsochrone(breaks = T1,res = 50,##Resolucion de OSM. 100 es el máximo pero tarda como 5 minutos
                                                osrm.profile = "car")
#plot(iso1_osm)
  #mapbox
#source("../../Reutilizables/Postgres_BUIG/token_mapbox.R")

iso1_mapbox=getIsochrones_mapbox(coord = punto_referencia_fijo |> unlist() |> paste(collapse = ","),
                     times =T1 )
#plot(iso1_mapbox)
  #Acc_SIGEH
source("codigos/SIGEH_isochrone.R")
tiempo_zona=accCost(T.GC, matrix(unlist(consulta$geometry |> st_transform(32614)),nrow = nrow(consulta),ncol = 2,byrow = T))

#plot(tiempo_zona)
# crs(tiempo_zona)=st_crs("EPSG:32614")$wkt
# tiempo_zona[ is.infinite(tiempo_zona)]=300
# tiempo_zona[ tiempo_zona==300]=NA
# projectRasterForLeaflet(tiempo_zona,method = "bilinear")
# leaflet() |> 
#   addTiles() |> 
#   addRasterImage(projectRasterForLeaflet(tiempo_zona,method = "ngb"),colors = colorNumeric("Spectral",domain = getValues(tiempo_zona) |> unique(),na.color = NA))

iso1_sigeh=raster::rasterToContour(tiempo_zona, levels = T1)
#plot(iso1_sigeh)
#Comparación
library(leaflet)
leaflet() |> 
  addTiles() |> 
  addMarkers(data=punto_referencia_fijo) |> 
  addPolygons(data=iso1_osm ,color='purple',fillColor = "purple",group = "osm") |> 
  addPolygons(data=iso1_mapbox |> st_as_sf() |> st_transform(st_crs("EPSG:4326")) ,color='green',fillColor = "green",group = "mapbox") |> 
  addPolylines(data=iso1_sigeh |> st_as_sf() |> st_set_crs(st_crs("EPSG:32614")) |>st_transform(st_crs("EPSG:4326")) ,color='blue',fillColor = "blue",group = "sigeh") |> 
  addLayersControl(overlayGroups =c("osm","mapbox","sigeh")) |> 
  #addRasterImage(tiempo_zona) |> 
  leafem::addMouseCoordinates()
