library(shiny)
library(leaflet)
library(leaflet.extras)

ui <- fluidPage(
  actionButton("add_poly", "Agregar Polígono Predefinido"),
  leafletOutput("map")
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-74.006, 40.7128, zoom = 12) %>%
      addDrawToolbar(
        targetGroup = "draw",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      )
  })
  
  observeEvent(input$add_poly, {
    lng <- c(-74.01, -74.01, -73.99, -73.99)
    lat <- c(40.71, 40.72, 40.72, 40.71)
    
    leafletProxy("map") %>%
      addPolygons(
        lng = lng, 
        lat = lat,
        fillColor = "blue",
        weight = 4,
        group = "draw" 
      )
  })
  
  observeEvent(input$map_draw_all_features, {
    print(input$map_draw_all_features)
  })
}

shinyApp(ui, server)