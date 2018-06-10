library(leaflet)
 # Print the map

function(input, output, session){
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=-122.4158391,15, lat=37.7892935, popup="San Francisco")
  })
}