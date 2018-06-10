library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)

breweryData <- read.csv('data/Brewery_Info.csv')

# unique beer styles
beer_types <- c("Berliner Weissbier", "Belgian Witbier", "Kolsch",
                "Blonde Ale", "Golden Ale", "Lager", "Hefeweizen", "Pilsner",
                "Saison", "Pale Ale", "Amber Ale", "IPA", "Double IPA", "Altbier",
                "Barleywine", "Brown Ale", "Rye Beer", "Scotch Ale", "Bock", 
                "Porter", "Stout", "Imperial Stout")

# beer icon 
icons <- list.files("www/beer_type", pattern="*.jpg")
beer_icons <- sapply(icons, function(x) paste("beer_type/", x, sep=""), USE.NAMES = FALSE)

###############
# UI 
###############

shinyUI(fluidPage(
  titlePanel(div("",
                 img(height = 90, 
                     src = "SFBW-2018-logo.png", class='pull-left'))),
    # dashboardPage(
    # dashboardHeader(),
    # dashboardSidebar(disable=T),
    # dashboardBody(
    tags$link(
      rel = "stylesheet", 
      href="https://fonts.googleapis.com/css?family=Lobster"
    ),
    tags$style("h1{font-family: 'Lobster'}"),
    h1('San Francisco Beer Week 2018', align = 'center', style = "font-size:48pt"),
    br(), br(),
  
    # BREWERY FEATURES FILTER 
    fixedRow(column(12, p('Brewery Filters:'),
                    style='padding:10px;height:30px;font-weight:bold;font-size:11pt')),
    fixedRow(style='height:20px',
      column(2, checkboxInput("food", tagList("Serve Food", icon("cutlery")), T))
      , column(2, checkboxInput("dogs", tagList("Dog Friendly", icon("paw")), F))
      , column(2, checkboxInput("happyhour", tagList("Happy Hour", icon("beer")), F))
      , column(2, checkboxInput("reservations", tagList("Reservations", icon("calendar-check-o")), F))
      , column(2, checkboxInput("outdoor", tagList("Outdoor Seating", icon("sun-o")), F))
      , column(2, checkboxInput("publictrans", tagList("Public Transportation", icon("bus")), F))
    ),
   
    # TRAVEL TIME & METHOD FILTER 
    fluidRow(style='height:100px'
             , column(2, dateInput("opendate", label="Open Date & Time:", value = "2018-01-02"))
             , column(10, sliderInput("opentime", label="", width="300px",
                                     min = as.POSIXct("12:00 AM", tz="America/Los_Angeles", format="%I:%M %p"),
                                     max = as.POSIXct("11:59 PM", tz="America/Los_Angeles", format="%I:%M %p"),
                                     value = c(as.POSIXct("9:00 AM", tz="America/Los_Angeles",
                                                          format="%I:%M %p")),
                                     timeFormat = "%I:%M %p", timezone="-0800"), animate=T, ticks=F)
             
    ), 
    fluidRow(
      br()
      , column(2,
               box(width = NULL, solidHeader = TRUE,
                   selectInput("mode", "Mode of Transport:",
                               c("Transit" = "transit", "Walking" = "walking", "Driving" = "driving"))
               ))
      , column(3, selectInput("origin", label="Origin", choices=breweryData$brewery))
      , column(3, selectInput("destination", label="Destination", choices=breweryData$brewery))
      # , column(2, style=' margin-top: 25px',submitButton("Direction"))
    ), 
    br(),
  
    # BEER TYPE FILETER
    fluidRow(
      column(2, checkboxGroupInput("beertype", "Beer Type:",
                                   choiceNames = mapply(beer_types, beer_icons, FUN=function(type, ic) {
                                     tagList(
                                       tags$img(src=ic, width=10, height=15), type
                                     )
                                   }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                                   choiceValues = beer_types
      )),
      column(10, leafletOutput("map", width="100%", height = 600))
    )
    

#     ) # end dashboardBody
   ) # end dashboardPage
) # end shinyUI
