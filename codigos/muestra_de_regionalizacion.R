##Muestra de regionalización
library(sf)
library(leaflet)

### Cargmos agebs e info demográfica. 

"../../Reutilizables/Demograficos/scince/tablas/cpv2020_ageb_urb_servicios_de_salud.dbf" |> 
  st_read()->ageb_salud
"../../Reutilizables/Demograficos/scince/ageb_urb.shp" |> st_read()->ageb_pob
ageb_pob=merge(ageb_pob,ageb_salud,by='CVEGEO')
ageb_pob=ageb_pob |> 
  dplyr::select(CVEGEO, POB1,POB42,POB84, dplyr::all_of(paste0("SALUD",1:10)),geometry) |> 
  st_transform(st_crs("EPSG:4326"))

loc_rural_pe=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13pe.shp") |> 
  st_transform(st_crs("EPSG:4326"))
loc_rural_pe=loc_rural_pe |> 
  dplyr::select(CVEGEO,geometry)
##A estos poligonos externos le agregamos los puntos de loc. rural que no estén
loc_rural_pr=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13lpr.shp") |> 
  st_transform(st_crs("EPSG:4326")) 
loc_rural_pr=loc_rural_pr |> 
  dplyr::select(CVEGEO,geometry) |> 
  dplyr::mutate(CVEGEO=substr(CVEGEO,1,9)) |> 
  st_buffer(100)

loc_rural=rbind(loc_rural_pe,loc_rural_pr |> dplyr::filter(!CVEGEO%in%loc_rural_pe$CVEGEO))

ageb_geo=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13a.shp") |> 
  st_transform(st_crs("EPSG:4326"))
ageb_geo=ageb_geo |> 
  dplyr::select(CVEGEO,geometry)

##De hecho, todavía se pueden identificar más geometrías para las localidades/AGEB.
#e.g. CUATLAPALAPA, Calnali. 
loc_urb=st_read("../../Reutilizables/Cartografia/Cartografia_2025/13_hidalgo/conjunto_de_datos/13l.shp") |> 
  st_transform(st_crs("EPSG:4326"))

leaflet() |> 
  addTiles() |> 
  addPolygons(data=ageb_geo,group = "ageb") |> 
  addPolygons(data=loc_rural,group = "loc_r") |> 
  addLayersControl(overlayGroups = c("ageb","loc_r"))


ERO=openxlsx::read.xlsx("inputs/Estudio_Regionalización_Operativa_2025.xlsx",3) 
ERO$colores=(openxlsx2::wb_get_cell_style(
  openxlsx2::wb_load("inputs/Estudio_Regionalización_Operativa_2025.xlsx",3),
  dims = paste0("A1:A", (nrow(ERO)+1) )))[-1]
memoria=ERO$colores[1]
grupos=list()

grupo=1
for(color in  ERO$colores){
  if(color!=memoria){
    #perdida de memoria
    memoria=color
    grupo=grupo+1
    grupos=append(grupos,grupo)
  }
  else{
    grupos=append(grupos,grupo)
  }
}
ERO$grupo=grupos |> unlist() 


ERO$AGEB=stringr::str_squish(ERO$AGEB)
ERO$AGEB=gsub("\\'",replacement = "",x =ERO$AGEB )
##Estaba raro que hubiera localidades urbanas sin agebs asignadas. Suponemos que quiere decir "Todas"
paste0(ERO$Nombre.Mpio[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)],
       ERO$Nombre.Localidad[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)])

paste0(ERO$Clave.Mpio.[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)],
       ERO$Clave.Localidad[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)] )
ERO$AGEB[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)][1]=
paste(substr(ageb_geo$CVEGEO[substr(ageb_geo$CVEGEO,1,9)%in%paste0("13",
                                                      paste0(ERO$Clave.Mpio.[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)][1],
                                                                  ERO$Clave.Localidad[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)][1] ))],10,13),collapse =' ')
ERO$AGEB[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)][1]=
paste(substr(ageb_geo$CVEGEO[substr(ageb_geo$CVEGEO,1,9)%in%paste0("13",
                                                      paste0(ERO$Clave.Mpio.[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)][1],
                                                                  ERO$Clave.Localidad[ERO$urbana.o.rural=='U' & is.na(ERO$AGEB)][1] ))],10,13),collapse =' ')
##Se ejecutan dos.
##También se nos hizo raro que localidades rurales tuvieran agebs asociados
ERO$AGEB[ERO$urbana.o.rural=='R' & !is.na(ERO$AGEB)]=NA

ERO_longer=ERO |> 
  dplyr::select(CLUES:grupo)



ERO_longer <- ERO |>
  dplyr::select(CLUES:grupo, AGEB) |> 
  tidyr::separate_wider_delim(
    cols = AGEB,
    delim = " ",
    names_sep = "_",      
    too_few = "align_start" 
  )

ERO_longer=ERO_longer |> 
  tidyr::pivot_longer(cols = AGEB_1:AGEB_44,values_to = "AGEB") |> 
  dplyr::filter(!is.na(AGEB))

ERO_previo_a_mapa=rbind(ERO |> dplyr::select(CLUES:grupo) |> 
                          dplyr::filter(urbana.o.rural=='R'),
                        ERO_longer |> dplyr::select(CLUES:grupo,AGEB))
##Te faltan las rurales de tipo poligono
ERO_previo_a_mapa=ERO_previo_a_mapa |> 
  dplyr::mutate(CVEGEO=ifelse(is.na(AGEB),paste0("13",Clave.Mpio.,Clave.Localidad),paste0("13",Clave.Mpio.,Clave.Localidad,AGEB))) |> 
  dplyr::group_by(CVEGEO,CLUES,grupo) |> 
  dplyr::slice_head(n=1) |> 
  merge(y=rbind(ageb_geo |> dplyr::select(CVEGEO,geometry),loc_rural |> dplyr::select(CVEGEO,geometry)),by='CVEGEO',all.x=T) |> 
  st_as_sf() |> 
  dplyr::mutate(tiene_geo=st_is_empty(geometry))


ERO_previo_a_mapa=ERO_previo_a_mapa |> 
  dplyr::filter(tiene_geo==F)



##Mapeo por grupos. 
ERO_previo_a_mapa$grupo |> unique() |> sort()##295 grupos
colores=rainbow(295) |> sample(size = 295,replace = F)
cabeceras_de_grupo=ERO_previo_a_mapa |> dplyr::group_by(grupo) |> dplyr::slice_head(n=1)
library(leaflet.extras)
leaflet() |> addTiles() |> 
  addPolygons(data=ERO_previo_a_mapa,color = colores[ERO_previo_a_mapa$grupo],fillColor = colores[ERO_previo_a_mapa$grupo],label = ERO_previo_a_mapa$grupo) |> 
  addPolygons(data = cabeceras_de_grupo,color =NA,fillColor = NA,group = "Cabezas_de_grupo",label = cabeceras_de_grupo$grupo) |> 
  addSearchFeatures(targetGroups = "Cabezas_de_grupo")





##Ahora por clasificamos las CLUES por grupo.
source("codigos/csv_to_geojson.R")
CLUES_previo_a_mapa=ERO_previo_a_mapa |> 
  st_drop_geometry() |> 
  dplyr::select(CLUES,grupo) |> 
  dplyr::group_by(CLUES,grupo) |> 
  dplyr::slice_head(n=1) |> 
  merge(y = clues_en_operacion |> dplyr::select(CLUES,NOMBRE.COMERCIAL,geometry),by='CLUES',all=T) |> 
  dplyr::filter(!is.na(grupo)) |> 
  st_as_sf()
  

leaflet() |> addTiles() |> 
  addPolygons(data=ERO_previo_a_mapa,color = colores[ERO_previo_a_mapa$grupo],fillColor = colores[ERO_previo_a_mapa$grupo],label = ERO_previo_a_mapa$grupo,group = "Grupos") |> 
  addPolygons(data = cabeceras_de_grupo,color =NA,fillColor = NA,group = "Cabezas_de_grupo",label = cabeceras_de_grupo$grupo) |> 
  addCircleMarkers(data = CLUES_previo_a_mapa ,color = colores[CLUES_previo_a_mapa$grupo],fillColor = colores[CLUES_previo_a_mapa$grupo],group = "CLUES",label = CLUES_previo_a_mapa$grupo) |> 
  addSearchFeatures(targetGroups = "Cabezas_de_grupo") |> 
  addLayersControl(overlayGroups = c("Grupos","CLUES"))
