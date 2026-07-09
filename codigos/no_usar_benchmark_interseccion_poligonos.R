###Exploración de benchmark. 
##St-intersection vs. st_filter vs. st_join

#usando los agebs y una bola random 

library(sf)

library(sf)
library(dplyr)
library(purrr)

generar_bola_aleatoria <- function(bbox) {
  cx <- runif(1, bbox["xmin"], bbox["xmax"])
  cy <- runif(1, bbox["ymin"], bbox["ymax"])
  punto <- st_sfc(st_point(c(cx, cy)), crs = 4326)
  
  ancho_max <- (bbox["xmax"] - bbox["xmin"]) * 0.10
  radio <- runif(1, ancho_max * 0.1, ancho_max)
  
  suppressWarnings(st_buffer(punto, dist = radio))
}

bbox_scince <- st_bbox(demograficos_scince)
n_iter <- 200

set.seed(42)
bolas_aleatorias <- replicate(n_iter, generar_bola_aleatoria(bbox_scince), simplify = FALSE)

resultados <- data.frame(
  st_filter = numeric(n_iter),
  st_join = numeric(n_iter),
  st_base = numeric(n_iter),
  st_intersection = numeric(n_iter)
)

# 3. Loop de iteración

for (i in 1:200) {
  if(i%%50==0){
    print(n_iter)
  }
  bola <- bolas_aleatorias[[i]] |> st_as_sf()
  
  # --- Método 1: st_filter ---
  t0 <- Sys.time()
  res_filter <- st_filter(demograficos_scince, bola)
  resultados$st_filter[i] <- as.numeric(Sys.time() - t0, units = "secs")
  
  # --- Método 2: st_join (Left = FALSE actua como Inner Join) ---
  t0 <- Sys.time()
  res_join <- st_join(demograficos_scince, bola, join = st_intersects, left = FALSE)
  resultados$st_join[i] <- as.numeric(Sys.time() - t0, units = "secs")
  
  # --- Método 3: Indexación Base [st_intersects, ] ---
  t0 <- Sys.time()
  # sparse = FALSE devuelve un vector lógico para indexar directamente
  res_base <- demograficos_scince[st_intersects(demograficos_scince, bola, sparse = FALSE), ]
  resultados$st_base[i] <- as.numeric(Sys.time() - t0, units = "secs")
  
  # --- Método 4: st_intersection
  t0 <- Sys.time()
  suppressWarnings({
    res_inter <- st_intersection(demograficos_scince, bola)
  })
  resultados$st_intersection[i] <- as.numeric(Sys.time() - t0, units = "secs")
}

summary(resultados)
