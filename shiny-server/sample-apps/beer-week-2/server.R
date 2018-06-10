library(shiny)
library(leaflet)

data = read.csv('data/Brewery_info.csv')
#take3

shinyServer(function(input, output){
    
  output$map <- renderLeaflet({
    leaflet() %>%
    addTiles(
      urlTemplate = "https://api.mapbox.com/styles/v1/yiqiang/cjb631its10852slahgl3ulex/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoieWlxaWFuZyIsImEiOiJjamI2MjJ2aDgzZTJiMzdvMTEza25vN3czIn0.da7McqdO9XgggUE0LTCx3Q"
    ) %>%
    fitBounds(lng1 =-122.517745, lat1 =  37.808235, lng2 = -122.387075, lat2 = 37.707238)
  })
  
  BeerIcon <- awesomeIcons(
      icon = 'beer',
      iconColor = 'white',
      library = 'ion',
      markerColor = "blue"
  )
  
  new_data <- reactive({
      if(input$food == T) {
          t <- subset(data,  data$Serve_Food == T & data$Beer_types %in% input$beertype)
      } else {
          t <- subset(data,  data$Beer_types %in% input$beertype)
      }
      return(t)
  })
  
  
  observe({
      tmp <- new_data()
      d <- dim(tmp)[1]
      if(!is.null(d)) {
      leafletProxy("map", data = tmp) %>% clearMarkers() %>%
          addAwesomeMarkers(~Long, ~Lat, icon = BeerIcon, layerId = 1)
          }
      
      # leafletProxy("map") %>% clearMarkers()
      # tmp <- new_data()
      # d <- dim(tmp)[1]
      # if(!is.null(d)){
      #     for(i in 1:nrow(tmp)){
      #             leafletProxy("map") %>% addAwesomeMarkers(lat = tmp$Lat[i], lng = tmp$Long[i],
      #                                                       icon = BeerIcon, layerId = 1)
      #     }
      # }
  })
  
  # Show a popup at the given location
  # showBeerPopup <- function(id, lat, lng) {
  #   content <- as.character(tagList(
  #     tags$h4("Brewery ID:"),
  #     tags$strong(), tags$br(),
  #     sprintf("Street"), tags$br(),
  #     sprintf("City, Zipcode"), tags$br(),
  #     sprintf("Beer Type")
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
  # }
  
  # When map is clicked, show a popup with Berewry info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_marker_click
  #   if (is.null(event))
  #     return()
  # 
  #   isolate({
  #     showBeerPopup(event$id,event$lat, event$lng)
  #   })
  # })
  
})
