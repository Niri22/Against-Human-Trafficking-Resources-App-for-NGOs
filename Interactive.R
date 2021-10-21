#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(readr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
CDI_lang_lat <- allCDI[sample.int(nrow(allCDI), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
CDI_lang_lat <- CDI_lang_lat[order(CDI_lang_lat$`Situational vulnerability Scores`),]



provincelst <- c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
                 "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
                 "Alberta", "British Columbia")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Choices for drop-downs
  vars <- c(
    "Centile score" = "Residential instability Scores",
    "College education" = "Economic dependency Scores",
    "Median income" = "Ethno-cultural composition Scores",
    "Population" = "Situational vulnerability Scores"
  ),
  
  
  navbarPage("The Canadian Index of Multiple Deprivation (CIMD)", id="nav",
             
             tabPanel("Interactive map",
                      
                      
                      # If not using custom CSS, set height of leafletOutput to a number instead of percent
                      leafletOutput("map", width="500", height="500"),
                      
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    
                                    h2("Canadian Index of Multiple Deprivation explorer"),
                                    
                                    selectInput("color", "Color", vars),
                                    selectInput("size", "Size", vars, selected = "Situational vulnerability Scores"),
                                    
                                    plotOutput("histCentile", height = 200),
                                    plotOutput("scatterCollegeIncome", height = 250)
                      ),
                      
                      tags$div(id="cite",
                               'Data compiled for ', tags$em('The Canadian Index of Multiple Deprivation (CIMD) 2016'
                               )
                      )
             ),
             
             tabPanel("Data explorer",
                      fluidRow(
                        column(3,
                               selectInput("Province", "Province", c("All province"="", structure(Province, names=Province), "Toronto, ON"="ON"), multiple=TRUE)
                        ),
                        column(3,
                               conditionalPanel("input.Province",
                                                selectInput("CD", "CDs", c("All CDs"=""), multiple=TRUE)
                               )
                        ),
                      )
             ),
             hr(),
             DT::dataTableOutput("ziptable")
  ),
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #load data#
  CDI_lang_lat <- read_csv("CDI_lang_lat.csv")
  CDI_lang_lat$Latitude <- as.numeric(CDI_lang_lat$Lattitude)
  CDI_lang_lat$Longitude <- as.numeric(CDI_lang_lat$Longitude)
  CDI_lang_lat$RIQ <- as.numeric(CDI_lang_lat$`Residential instability Quintiles`)
  CDI_lang_lat$EDQ <- as.numeric(CDI_lang_lat$`Economic dependency Quintiles`)
  CDI_lang_lat$ECQ <- as.numeric(CDI_lang_lat$`Ethno-cultural composition Quintiles`)
  CDI_lang_lat$SVQ <- as.numeric(CDI_lang_lat$`Situational vulnerability Quintiles`)
  CDI_lang_lat=filter(CDI_lang_lat, Latitude != "NA") # removing NA values
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -79.34482, lat = 43.68866, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(zipsInBounds()$centile,
         breaks = centileBreaks,
         main = "SuperZIP score (visible zips)",
         xlab = "Percentile",
         xlim = range(allzips$centile),
         col = '#00DD00',
         border = 'white')
  })
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }
    
    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    }
    
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Adult population: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
