library(sf)
library(leaflet)

### Cargmos agebs e info demográfica. 

###Fuente: SCINCE (AGEB)
"../../Reutilizables/Demograficos/scince/tablas/cpv2020_ageb_urb_servicios_de_salud.dbf" |> 
  st_read()->ageb_salud
"../../Reutilizables/Demograficos/scince/ageb_urb.shp" |> st_read()->ageb_pob

ageb_pob=merge(ageb_pob,ageb_salud,by='CVEGEO')
ageb_pob=ageb_pob |> 
  dplyr::select(CVEGEO, POB1,POB42,POB84, dplyr::all_of(paste0("POB",17:25)), dplyr::all_of(paste0("SALUD",1:10)),geometry) |> 
  st_transform(st_crs("EPSG:4326"))
####Localidades rurales
loc_rural_pe=st_read("../../Reutilizables/Demograficos/scince/loc_rur.shp") |> 
  st_transform(st_crs("EPSG:4326"))
loc_rural_p_salud=st_read("../../Reutilizables/Demograficos/scince/tablas/cpv2020_loc_rur_servicios_de_salud.dbf")
loc_rural_pe=loc_rural_pe |> merge(loc_rural_p_salud,by='CVEGEO')

####Localidades urbanas y rurales
loc_urb=st_read("../../Reutilizables/Demograficos/scince/loc_urb.shp") |> 
  st_transform(st_crs("EPSG:4326"))
loc_urb_p_salud=st_read("../../Reutilizables/Demograficos/scince/tablas/cpv2020_loc_urb_servicios_de_salud.dbf")
loc_urb=loc_urb |> merge(loc_urb_p_salud)

loc_rural_urb=
  loc_urb |> dplyr::select(CVEGEO,NOM_MUN,NOMGEO,POB1,POB42,POB84, dplyr::all_of(paste0("POB",17:25)), dplyr::all_of(paste0("SALUD",1:10))) |> dplyr::mutate(CVE_AGEB=NA) |> 
  rbind(loc_rural_pe |> dplyr::select(CVEGEO,NOM_MUN,NOMGEO,CVE_AGEB,POB1,POB42,POB84, dplyr::all_of(paste0("POB",17:25)),, dplyr::all_of(paste0("SALUD",1:10))  
  ) |> dplyr::filter(!CVEGEO%in%loc_urb$CVEGEO)|> st_buffer(100))




loc_rural_pe |> dplyr::select(CVEGEO,NOM_MUN,NOMGEO,CVE_AGEB,POB1,POB42,POB84, dplyr::all_of(paste0("POB",17:25)),, dplyr::all_of(paste0("SALUD",1:10))  
) |> st_buffer(100) |> 
  rbind(loc_urb |> dplyr::select(CVEGEO,NOM_MUN,NOMGEO,POB1,POB42,POB84, dplyr::all_of(paste0("POB",17:25)), dplyr::all_of(paste0("SALUD",1:10))) |> 
          dplyr::mutate(CVE_AGEB=NA))
##Los podemos juntar y llamarlos demograficos en general
#demograficos_scince=plyr::rbind.fill(ageb_pob,loc_rural_urb) 


ageb_pob |> st_simplify(preserveTopology = T,dTolerance = 75) |> st_write("outputs/ageb_pob75.geojson",driver="GeoJSON")
loc_rural_pe |> st_buffer(100) |> st_simplify(preserveTopology = T,dTolerance = 50) |> st_write("outputs/loc_rural50.geojson",driver="GeoJSON")
loc_rural_pe |> st_buffer(100) |> st_write("outputs/loc_rural100.geojson",driver="GeoJSON")

leaflet() |> addTiles() |> 
  addPolygons(data=ageb_pob,group = "AGEB") |> 
  addPolygons(data=loc_urb,group = "locs_urb") |> 
  addPolygons(data=loc_rural_pe |> st_buffer(100),group = "locs_rur") |> 
  addLayersControl(overlayGroups = c("AGEB","locs_urb","locs_rur"))
##Definiremos los demográficos como agebs U loc_rural en buffer.
ageb_pob=ageb_pob |> dplyr::mutate(CVE_AGEB=substr(CVEGEO,10,13)) |> 
  dplyr::mutate(CVEGEO=substr(CVEGEO,1,9))|> merge(loc_urb |> dplyr::select(CVEGEO,NOM_MUN,NOMGEO) |> st_drop_geometry(),by="CVEGEO",all.x=T) |> 
  dplyr::mutate(CVEGEO=ifelse(!is.na(CVE_AGEB),paste0(CVEGEO,CVE_AGEB),CVEGEO ))
demograficos_scince=rbind(ageb_pob 
                          # |> dplyr::mutate(
  #NOM_MUN=NA,NOMGEO=NA,
  #CVE_AGEB=NA)
,loc_rural_pe|> dplyr::select(CVEGEO,NOM_MUN,NOMGEO,CVE_AGEB,POB1,POB42,POB84, dplyr::all_of(paste0("POB",17:25)),, dplyr::all_of(paste0("SALUD",1:10))  
) |> st_buffer(150)) 
demograficos_scince |> st_simplify(preserveTopology = T,dTolerance = 20) |> st_write("outputs/demograficos_cartograficos_scince_20.geojson",driver='GeoJSON',append = F,delete_dsn = T)

demograficos_scince=st_read("outputs/demograficos_cartograficos_scince_20.geojson")
leaflet() |> addTiles() |> 
  addPolygons(data=demograficos_scince,group = "AGEB")
#ageb_pob contiene la geometría e información poblacional básica y de salud. 
##Incluye: 
##Población total
##Población hombres
##Población mujeres
##Población 3+
##Población 5+
##Población 12+
##Población 15+
##Población 18+
##Población 25+
##Población 60+
##Población 65+
##Población 70+

##Fuente: Marco geo... INEGI 2025 dic
loc_rural_pe=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13pe.shp") |> 
  st_transform(st_crs("EPSG:4326"))
loc_rural_pe=loc_rural_pe |> 
  dplyr::select(CVEGEO,geometry)
##A estos poligonos externos le agregamos los puntos de loc. rural que no estén
loc_rural_pr=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13lpr.shp") |> 
  st_transform(st_crs("EPSG:4326")) 
loc_rural_pr=loc_rural_pr |> 
  dplyr::select(CVEGEO,geometry)

loc_rural=rbind(loc_rural_pe,loc_rural_pr |> dplyr::filter(!CVEGEO%in%loc_rural_pe$CVEGEO))
loc_rural$CVEGEO |> unique() |> length()
ageb_geo=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13a.shp") |> 
  st_transform(st_crs("EPSG:4326"))
ageb_geo=ageb_geo |> 
  dplyr::select(CVEGEO,geometry)

##Notamos que hay unas pocas más de agebs en 2025 vs 2020. El tema va a ser que no tendremos población. 


##De hecho, todavía se pueden identificar más geometrías para las localidades/AGEB.
#e.g. CUATLAPALAPA, Calnali. 
loc_urb=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13l.shp") |> 
  st_transform(st_crs("EPSG:4326"))

cartograficos_inegi=loc_urb |> dplyr::select(CVEGEO,geometry) |> 
  rbind(loc_rural |> dplyr::select(CVEGEO,geometry) |> dplyr::filter(!CVEGEO%in%loc_urb$CVEGEO) |> st_buffer(100)) |> 
  rbind(ageb_geo |> dplyr::select(CVEGEO,geometry)) 
##A los datos georeferenciados de 2025 les agregamos la info demográfica del scince. 

demograficos_scince$CVEGEO |> unique() |> length()
cartograficos_inegi$CVEGEO |> unique() |> length()

###Está dificil hacerlos matchear. Seguimos con puro scince hasta que tenga tiempo de hacer algo mejor. 



!(demograficos_scince$geometry |> st_is_empty()) |> all()
!(cartograficos_inegi$geometry |> st_is_empty()) |> all()
demograficos_scince 
demograficos_scince=demograficos_scince
demograficos_scince |> write_sf("outputs/demograficos_cartograficos_scince.shp")

demograficos_scince |> lapply(class)
demograficos_scince$geometry =demograficos_scince$geometry$x
demograficos_scince |> st_write("outputs/demograficos_cartograficos_scince_or.geojson",driver="GeoJSON")

st_read("../../Reutilizables/Cartografia/municipiosjair.shp") |> lapply(class)

leaflet() |> addTiles() |> addPolygons(data=demograficos_scince)
