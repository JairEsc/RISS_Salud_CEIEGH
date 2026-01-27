# #Código mínimo para modelo de fricción
# library(raster)
# uso_de_suelo=raster("inputs/accesibilidad_SIGEH/uso_de_suelo_friccion.tif")
# pendiente=raster("inputs/accesibilidad_SIGEH/pendiente.tif")
# carreteras=raster("inputs/accesibilidad_SIGEH/carreteras.tif")
# #Sí me voy a tomar la libertad de actualizar los valores del raster que estén cerca de 90 grados
# pendiente[pendiente<120 & pendiente>=90]=120
# pendiente[pendiente<=90 & pendiente>50]=50
# ####Accesibilidad a pie
# slp_walk = 6 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.05))  # Calcula la velocidad de caminata ajustada por la pendiente.
# #plot(-90:90,6*exp(-0.4*abs(tan(-90:90*pi/180)))+0.05,'l',ylab='Velocidad km/h',main='Velocidad caminando en función de la pendiente',xlab='Grados')
# terrain_walk_spd = uso_de_suelo * slp_walk       #Le quité el /5.0. Quiero pensar que es la velocidad de caminata según uso de suelo. El promedio es de 5.5 km/h         # Calcula la velocidad sobre el terreno ajustada por la pendiente y el uso de suelo.
# 
# ##########Accesibilidad por carreteras
# slp_car = 50 * exp(-0.4 * abs(tan(pendiente * pi / 180) + 0.12))  # Calcula la velocidad sobre carreteras ajustada por la pendiente.
# 
# sloped_road_spd = carreteras * slp_car / 50.0 # Calcula la velocidad ajustada por pendiente para carreteras y la convierte en un raster.
# 
# 
# merged_spd = merge(sloped_road_spd, terrain_walk_spd)     # Combina los rasters de velocidad de carreteras y terreno.
# 
# friction = 1.0 / (merged_spd * 1000 / 60.0 ) 
# #friction_4326=projectRaster(friction)
library(gdistance)
# Trans = transition(friction, function(x) 1 / mean(x), 8)  # Crea una matriz de transición basada en la fricción.
# T.GC = geoCorrection(Trans, type="c") 
# 
# # hidalgo=st_read("inputs/accesibilidad_SIGEH/hidalgo/LIM_MUNICIPALES.shp")
# # n=15
# # lugares_destino_ficticios=st_sample(hidalgo$geometry,n)
# # tiempo_zona = accCost(T.GC, matrix(unlist(lugares_destino_ficticios),nrow = n,ncol = 2,byrow = T))  # Calcula el costo acumulado desde un punto de inicio (coordenadas especificadas) usando la matriz de transición corregida (T.GC).
# # plot(tiempo_zona)
# # plot(lugares_destino_ficticios,add=T)
# #accCost(T.GC, st_point(c(-98.5,20)) |> st_sfc(crs = 4326) |> st_transform(32614) |> unlist() ) |> plot()
# 
# saveRDS(object = T.GC, file = "inputs/accesibilidad_SIGEH/accesibilidad_carretera.rds")
T.GC=readRDS("inputs/accesibilidad_SIGEH/accesibilidad_carretera.rds")
#