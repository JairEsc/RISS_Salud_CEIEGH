#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

#Idea: Generalizar al menos 3 funciones de accesibilidad para su intergración por API
#Su publicación como api puede ser a través de docker.

#api1: msg
  #Regresa el tiempo en minutos entre dos puntos de acuerdo a su accesibilidad
#api2: plot
  #Regresa una imagen png del raster generado a partir de un input de puntos
#api3: isocronas
  #A partir de un punto (o varios) y una elección de cortes de tiempo, regresa las curvas de nivel del raster de accesibilidad

#* @apiTitle Accesibilidad SIGEH API
#* @apiDescription Funciones generalizadas para el servicio de cálculo de medidas de accesibilidad carretera en el estado de Hidalgo

#* Echo back the input//
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function(long,lat) {
  #long,lat= -98.5,20
    z=accCost(T.GC, st_point(c(as.numeric(long),as.numeric(lat))) |> 
                st_sfc(crs = 4326) |> 
                st_transform(32614) |> 
                unlist() 
              ) 
  
    plot(z)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}

