library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)
library(jsonlite)
library(shinydashboard)
library(htmltools)
library(lubridate)

# setwd("~/Desktop/beerWeek/Examples/beer-week-3")

###############
# Process Data
###############

breweryData <- read.csv('data/Brewery_Info.csv')
# breweryData[is.na(breweryData)] <- 0

# BEER TYPES
# convert beer types string to list
breweryData$beer_lst <- sapply(as.character(breweryData$beer_types), function(x) strsplit(x, ",")) %>% sapply(trimws)

# high-level beer types
# beer_types <- strsplit(as.character(breweryData$beer_types), ",") %>% unlist() %>% sapply(trimws) %>% unique()
beer_types <- c("Berliner Weissbier", "Belgian Witbier", "Kolsch",
                "Blonde Ale", "Golden Ale", "Lager", "Heifenweizen", "Pilsner",
                "Saison", "Pale Ale", "Amber Ale", "IPA", "Double IPA", "Altbier",
                "Barleywine", "Brown Ale", "Rye Beer", "Scotch Ale", "Bock", 
                "Porter", "Stout", "Imperial Stout")
# beer icon 
icons <- list.files("www/beer_type", pattern="*.jpg")
beer_icons <- sapply(icons, function(x) paste("beer_type/", x, sep=""), USE.NAMES = FALSE)

# OPENING DATE & TIME
# function to convert time
process_time <- function(text){
  if(is.null(text) | text =="") {text <- "-1"}
  if (text=="-1") {return (text)}
  else {
    t <- strsplit(text, split="-")[[1]]
    if(t[2]>"24:00") {t[2] <- "24:00"}
    return(t)
  }
}

# combine all opening hours to a column 
breweryData$all_hours <- select(breweryData, starts_with("hour")) %>% apply(1, paste, collapse=",") # combine all opening hours to a str
breweryData$all_hours <- lapply(breweryData$all_hours, function(x) unlist(strsplit(x, ","))) # convert to a vector
# breweryData$all_hours <- lapply(breweryData$all_hours, function(x) lapply(x, process_time)) # convert to 12:00 system

# function to determine if open
if_store_open <- function(input, h){
  if(length(h) == 1) {
      return (-1)}
  else {
    input <- input - hours(8)
    input_time <- format(input, "%H:%M")
    if(h[2] == "00:00") {h[2] <- "25:00"}
    if(input_time >= h[1] & input_time < h[2]) {output <- 1}
    else {output <- -1}
  }
  return(output)
}


# PATH DATA
json <- readLines("data/Output.txt", warn = FALSE) %>%
  paste(collapse = "\n") %>%
  fromJSON(simplifyVector = FALSE)

showPath <- function(click1, click2){
  id1 = click1[1]
  id2 = click2[1]
  path_name = paste0(click1[1], '_', click2[1], '_', 'walking')
  json_tmp = json[[path_name]]
  json_tmp
}

###############
# Server
###############

shinyServer(function(input, output){
  
  add_labels <- function(df) {
    labels <- sprintf(
      "<h5><a href='%s'>%s</a></h5></br>Address: %s, %s %s</br>Tel: %s</br>",
      df$website, df$brewery, df$street, df$city, df$zipcode, df$phone_number 
    ) %>% lapply(htmltools::HTML)
    return(labels)
  }
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/yiqiang/cjb631its10852slahgl3ulex/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoieWlxaWFuZyIsImEiOiJjamI2MjJ2aDgzZTJiMzdvMTEza25vN3czIn0.da7McqdO9XgggUE0LTCx3Q"
      ) %>%
      addMarkers(
        data = breweryData, lat = ~latitude, lng = ~longitude, layerId = ~brewery_id
      ) %>% 
      fitBounds(lng1=-122.517745, lat1 = 37.808235, lng2 = -122.186932, lat2 = 37.716482) %>% 
      setView(lat=37.777659, lng=-122.377363, zoom=12)
  })
  
  BeerIcon <- awesomeIcons(
    icon = 'beer',
    iconColor = '#ffd700',
    library = 'ion',
    markerColor = "black"
  )
  
  filtered_breweryData <- reactive({
    # filter for beer styles
    t <- breweryData[sapply(breweryData$beer_lst, function(x) all(input$beertype %in% x)),]
    
    # filter for opening hours 
    if(input$open_only){
      input_dow <- wday(as.Date(input$opendate, '%Y-%m-%d'), week_start=getOption("lubridate.week.start", 1))
      t$selected_dow <- lapply(t$all_hours, function(x) x[[input_dow]])
      t$if_open <- lapply(t$selected_dow, function(x) if_store_open(input$opentime, x))
      t <- subset(t, t$if_open == 1)
    }
    
    # filter for other features
    if(input$food == T) {t <- subset(t, t$serves_food == T)}
    if(input$dogs == T){t <- subset(t, t$dogs == T)}
    if(input$happyhour == T){t <- subset(t, t$happy_hour == T)}
    if(input$reservations == T){t <- subset(t, t$reservations == T)}
    if(input$outdoor == T){t <- subset(t, t$outdoor == T)}
    if(input$publictrans == T){t <- subset(t, t$public_transport == T)}
    
    return(t)
  })
  
  observe({
    leafletProxy("map") %>% clearMarkers()
    tmp <- filtered_breweryData()
    d <- dim(tmp)[1]
    if(!is.null(d)){
      labels <- add_labels(tmp)
      for(i in 1:nrow(tmp)){
        leafletProxy("map") %>% 
          addAwesomeMarkers(lat = tmp$latitude[i], lng = tmp$longitude[i], layerId = tmp$brewery_id[i],
                            icon = BeerIcon, popup = labels[i], 
                            labelOptions(opacity=1, textsize="20px")
                            )
      }
    }
  })
  
  # Function to format time as 12:00 system
  format_time <- function(text) {
    if(is.null(text) | text == "") {text <- "-1"}
    if (text =="-1") {return("Closed")}
    else {
      t <- strsplit(text, split="-")[[1]]
      if(t[2]>"24:00") {t[2] <- "24:00"}
      t <- sapply(t, function(x) format(strptime(x, "%H:%M"), format="%I:%M %P"))
      output <- paste(t[1], t[2], sep="-")
      return(output)
    }
  }
  
  # Helper functions to add opening hours to table
  add_openinghr1 <- function(lst){
    text <- sprintf("
                    <p>Mon: %s</p>
                    <p>Tue: %s</p>
                    <p>Wed: %s</p>
                    <p>Thur: %s</p>
                    ",
                    lst[[1]], lst[[2]], lst[[3]], lst[[4]]
    ) %>% lapply(htmltools::HTML)
    return(text)
  }
  
  add_openinghr2 <- function(lst){
    text <- sprintf("
                    <p>Fri: %s</p>
                    <p>Sat: %s</p>
                    <p>Sun: %s</p>
                    ",
                    lst[[5]], lst[[6]], lst[[7]]
    ) %>% lapply(htmltools::HTML)
    return(text)
  }
  
  
  # Filtered Table + 12:00PM
  filtered_breweryData_tbl <- reactive({
    t <- filtered_breweryData()
    t$address <- paste(t$street, ", ", t$city, " ", t$zipcode)
    
    # convert to 12:00 system
    t$all_hours <- select(t, starts_with("hour")) %>% apply(1, paste, collapse=",")
    t$all_hours <- lapply(t$all_hours, function(x) unlist(strsplit(x, ",")))
    t$all_hours <- lapply(t$all_hours, function(x) lapply(x, format_time)) 
    t$all_hours_wd <- lapply(t$all_hours, function(x) add_openinghr1(x))
    t$all_hours_wk <- lapply(t$all_hours, function(x) add_openinghr2(x))
    
    selected_cols <- c("brewery", "address", "phone_number", "all_hours_wd", "all_hours_wk")
    t <- t[, colnames(t) %in% selected_cols]
    colnames(t) <- c("Berwery", "Tel", "Address", "Opening Hours", ".")
    return(t)
  })
  
  output$brewery_tbl <- DT::renderDataTable({
      datatable(filtered_breweryData_tbl(),
                options = list(lengthMenu = c(3, 5, 10, 15, 20), pageLength = 3))
  })
  
  # show path
  rv <- reactiveValues()
  observeEvent(input$map_click,
               {rv$click1 <- NULL
               rv$click2 <- NULL
               leafletProxy("map") %>% clearGeoJSON()})
  
  observeEvent(input$map_marker_click, {
    leafletProxy("map") %>% clearGeoJSON()
    event <- input$map_marker_click
    if(is.null(event))
      return()
    if(is.null(rv$click1)){
      rv$click1 <- event
    }else{
      rv$click2 <- event
    }
    if(is.null(rv$click1) | is.null(rv$click2)){
      print(0)
    }else{
      json_tmp <- showPath(rv$click1[1], rv$click2[1])
      leafletProxy("map") %>% addGeoJSON(json_tmp, fill = F)
    }
  })
  
})

