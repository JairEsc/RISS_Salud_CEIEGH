source("codigos/csv_to_geojson.R")
source("codigos/definicion_cartografia_demografia.R")

con <- DBI::dbConnect(RSQLite::SQLite(), "clues_demograficos_municipios.sqlite")
##Falta agregar datos calculados a los geojsons

st_write(clues_en_operacion_s, con, "clues_en_operacion", delete_layer = FALSE)
st_write(limites_municipales |> st_transform(4326), con, "limite_municipal", delete_layer = FALSE)
st_write(demograficos_scince |> st_transform(4326), con, "demograficos_scince", delete_layer = FALSE)



