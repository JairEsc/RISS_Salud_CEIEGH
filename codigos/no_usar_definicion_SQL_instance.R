install.packages('RSQLite')
install.packages('nycflights13')
library(RSQLite)
con <- DBI::dbConnect(RSQLite::SQLite(), "my_flights.sqlite")
dplyr::copy_to(con, nycflights13::flights, "flights",
        temporary = FALSE, 
        indexes = list(
          c("year", "month", "day"), 
          "carrier", 
          "tailnum",
          "dest"
        )
)
flights_db <- dplyr::tbl(con, "flights")##Conexion esperada

################
source("codigos/csv_to_geojson.R")
source("codigos/definicion_cartografia_demografia.R")

con <- DBI::dbConnect(RSQLite::SQLite(), "clues_en_operacion_y_limites_municipales.sqlite")

st_write(clues_en_operacion_s, con, "clues_en_operacion", delete_layer = FALSE)
st_write(limites_municipales |> st_transform(4326), con, "limite_municipal", delete_layer = FALSE)
st_write(demograficos_scince |> st_transform(4326), con, "demograficos_scince", delete_layer = FALSE)



