library(shiny)
library(leaflet)

shinyUI(fluidPage(
  # text boxes to provide time
    
    fluidRow(
        column(2, textInput("start_time", 'Start time', value = "9:00", width = 75, placeholder = NULL)),
        column(2, selectInput("start_hour", " ", c('PM','AM'), width = 75, size = NULL)),
        column(2,textInput("end_time", 'End time', value = "1:00", width = 75, placeholder = NULL)),
        column(2, selectInput("end_hour", " ", c('AM','PM'), width = 75, size = NULL)),
        column(4, selectInput("dow", "Day", c('Monday','Tuesday', 'Wednesday', 'Thursday', 
                                              'Friday', 'Saturday', 'Sunday'), width = 150, size = NULL))
    ),
    
    fluidRow(
        column(2, checkboxInput("food", "Serve Food", T)),
        column(2, checkboxInput("dogfriendly", "Dog Friendly", F))
    ),
    
    fluidRow(
        column(2, checkboxGroupInput("beertype","Beer Type",  c('Craft', 'Ale'))),
        column(10, leafletOutput("map"))
    )
))
