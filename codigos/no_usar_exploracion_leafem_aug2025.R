library(sf)
library(leaflet)
library(leafem)
# Example data ##########
breweries91 <- st_as_sf(breweries91)
lines <- st_as_sf(atlStorms2005)
polys <- st_as_sf(gadmCHE)

# View settings ##########
view_settings <- list(
  "Base_tiles1" = list(
    coords = c(20, 50),
    zoom = 3
  ),
  "Base_tiles2" = list(
    coords = c(-110, 50),
    zoom = 5
  ),
  "breweries91" = list(
    coords = as.numeric(st_coordinates(st_centroid(st_union(breweries91)))),
    zoom = 8
  ),
  "atlStorms2005" = list(
    coords = as.numeric(st_bbox(lines)),
    options = list(padding = c(110, 110))
  ),
  "gadmCHE" = list(
    coords = as.numeric(st_bbox(polys)),
    options = list(padding = c(2, 2)),
    fly = TRUE
  )
)

# Opacity control settings ##########
opacityControl <- list(
  "breweries91" = list(
    min = 0,
    max = 1,
    step = 0.1,
    default = 1,
    width = '100%',
    class = 'opacity-slider'
  )
)

# Legends ##########
legends <- list(
  "breweries91" = "Legend for breweries"
)

leaflet() %>%
  ## Baselayer
  addTiles(group = "Base_tiles1") %>%
  addProviderTiles("CartoDB", group = "Base_tiles2") %>%
  
  ## Overlays
  addCircleMarkers(data = breweries91, group = "breweries91") %>%
  addPolylines(data = lines, group = "atlStorms2005") %>%
  addPolygons(data = polys, group = "gadmCHE") %>%
  
  ## LayersControl
  addLayersControl(
    baseGroups = c("Base_tiles1", "Base_tiles2"),
    overlayGroups = c("breweries91", "atlStorms2005", "gadmCHE"),
    options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
  ) %>%
  
  ## Customize Layers Control
  customizeLayersControl(
    view_settings = view_settings,
    home_btns = TRUE,
    home_btn_options = list(
      "Base_tiles1" = list(
        text = fontawesome::fa("home"),
        cursor = 'ns-resize',
        class = 'homebtn'
      ),
      "Base_tiles2" = list(
        text = fontawesome::fa("home"),
        cursor = 'pointer'
      ),
      "atlStorms2005" = list(
        text = fontawesome::fa("wind"),
        cursor = 'all-scroll'
      ),
      "breweries91" = list(
        text = fontawesome::fa("beer-mug-empty"),
        styles = 'background-color: lightgreen; float: inline-end'
      ),
      "gadmCHE" = list(
        text = fontawesome::fa("mountain"),
        styles = 'float: none;'
      )
    ),
    opacityControl = opacityControl,
    includelegends = TRUE,
    addCollapseButton = F,
    layersControlCSS = list("opacity" = 0.6),
    increaseOpacityOnHover = TRUE
  )