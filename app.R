#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(leaflet)
library(DT)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(maps)
library(plotly)
library(colorspace)
library(rsconnect)



vars <- list("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
             "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
             "Alberta", "British Columbia")
vars2 <- list("Canada","Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
             "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", 
             "Alberta", "British Columbia")

ui <- fluidPage(
    
    titlePanel("Indexes and Trafficking Corridor"),
    navbarPage("Menu", id="main",
              
               tabPanel("Index of Multiple Deprivation", 
                        navbarPage("Menu", id="Index of Multiple Deprivation",
                                   tabPanel("Index of Multiple Deprivation Interactive Map",
                                            pageWithSidebar(
                                                headerPanel('Index of Multiple Deprivation'),
                                                sidebarPanel(
                                                    selectInput(inputId = 'province', 'Choose a Province', vars, selected = vars[[2]]),
                                                    plotOutput("hist", height = 550)
                                                ),
                                                mainPanel(
                                                    navlistPanel(
                                                        tabPanel("Information about the Index", DT::dataTableOutput("Info")),
                                                        tabPanel("Residential Instability", leafletOutput("RIQ", height=700)),
                                                        tabPanel("Economic Dependency", leafletOutput("EDQ", height=700)),
                                                        tabPanel("Ethno Cultural Composition", leafletOutput("ECQ", height=700)),
                                                        tabPanel("Situational Vulnerability", leafletOutput("SVQ", height=700))
                                                        )
                                                
                                                )
                                            )
                                   ), 
                                   tabPanel("Index of Multiple Deprivation Data Explorer", DT::dataTableOutput("dataCDI"))
                        )
                   
               ),
               
               tabPanel("Trafficking Corridor", 
                        navbarPage("Menu",id = "Trafficking Corridor",
                            tabPanel("Trafficking Corridor Interactive Map", plotlyOutput("Migration")),
                            tabPanel("Trafficking Corridor Data Explorer", DT::dataTableOutput("dataTC")))
                        )
                        
               
        ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    #load data#
    
    CDI_lang_lat <- read_csv("CDI_lang_lat.csv")
    CDI_lang_lat$Latitude <- as.numeric(CDI_lang_lat$Lattitude)
    CDI_lang_lat$Longitude <- as.numeric(CDI_lang_lat$Longitude)
    CDI_lang_lat$RIQ <- as.numeric(CDI_lang_lat$`Residential instability Quintiles`)
    CDI_lang_lat$EDQ<- as.numeric(CDI_lang_lat$`Economic dependency Quintiles`)
    CDI_lang_lat$ECQ <- as.numeric(CDI_lang_lat$`Ethno-cultural composition Quintiles`)
    CDI_lang_lat$SVQ <- as.numeric(CDI_lang_lat$`Situational vulnerability Quintiles`)
    CDI_lang_lat=filter(CDI_lang_lat, Latitude != "NA") # removing NA 
    NetworkLoc_2_2 <- read_csv("NetworkLoc 2 2.csv")
    Location <- read.csv("Location.csv")
    Info <- read.csv("CIMD_indicators 2.csv")
    
    
    output$Info  <-DT::renderDataTable(datatable(
        Info[,c(1:4)],filter = 'top', class = 'cell-border stripe', 
        colnames = c("Indicators" , "Description" , "Variables", "Relations to Human Trafficking Indicators"
        )
    ))
    
    ###CD Indicators tab##
    
    mapdata <- reactive({ subset(CDI_lang_lat, CDI_lang_lat$Province == input$province)})
    
    # create a color paletter for category type in the data file
    pal_Ri <- colorFactor(pal = sequential_hcl(5), domain = c("1", "2", "3", "4", "5"))
    pal_Ed <- colorFactor(pal = sequential_hcl(5, h = 0, c = c(100, 0), l = 65, power = 1), domain = c("1", "2", "3", "4", "5"))
    pal_Ec <- colorFactor(pal = sequential_hcl(5, h = 40, c = c(100, 0), l = 65, power = 1), domain = c("1", "2", "3", "4", "5"))
    pal_Sv <- colorFactor(pal = sequential_hcl(5, h = 200, c = c(100, 0), l = 65, power = 1), domain = c("1", "2", "3", "4", "5"))
    #Outplots#
    
    
    # create the leaflet map  
    output$RIQ <- renderLeaflet({
        leaflet(mapdata()) %>% 
            addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
            addTiles() %>%
            addCircleMarkers(data = mapdata(), lat =  ~Latitude, lng =~Longitude, 
                             radius = 5, popup =~ paste0("PRCDDA: ", PRCDDA, "| Censor Division Name: ", `CDname/DRnom`,
                                                         "| Population: ", `Dissemination area (DA) Population`,
                                                         "| Residential instability Score: ", `Residential instability Scores`),
                             color = ~pal_Ri(RIQ),
                             stroke = FALSE, fillOpacity = 0.8)%>%
            addLegend(pal=pal_Ri, values=mapdata()$RIQ,opacity=1, na.label = "Not Available")%>%
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    output$EDQ <- renderLeaflet({
        leaflet(mapdata()) %>% 
            addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
            addTiles() %>%
            addCircleMarkers(data = mapdata(), lat =  ~Latitude, lng =~Longitude, 
                             radius = 5, popup =~ paste0("PRCDDA: ", PRCDDA, "| Censor Division Name: ", `CDname/DRnom`,
                                                         "| Population: ", `Dissemination area (DA) Population`,
                                                         "| Economic dependency Score: ", `Economic dependency Scores`),
                             color = ~pal_Ed(EDQ),
                             stroke = FALSE, fillOpacity = 0.8)%>%
            addLegend(pal=pal_Ed, values=mapdata()$EDQ,opacity=1, na.label = "Not Available")%>%
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    output$ECQ <- renderLeaflet({
        leaflet(mapdata()) %>% 
            addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
            addTiles() %>%
            addCircleMarkers(data = mapdata(), lat =  ~Latitude, lng =~Longitude, 
                             radius = 5, popup =~ paste0("PRCDDA: ", PRCDDA, "| Censor Division Name: ", `CDname/DRnom`,
                                                         "| Population: ", `Dissemination area (DA) Population`,
                                                         "| Ethno-cultural composition Score: ", `Ethno-cultural composition Scores`),
                             color = ~pal_Ec(ECQ),
                             stroke = FALSE, fillOpacity = 0.8)%>%
            addLegend(pal=pal_Ec, values=mapdata()$ECQ,opacity=1, na.label = "Not Available")%>%
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    output$SVQ <- renderLeaflet({
        leaflet(mapdata()) %>% 
            addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
            addTiles() %>%
            addCircleMarkers(data = mapdata(), lat =  ~Latitude, lng =~Longitude, 
                             radius = 5, popup =~ paste0("PRCDDA: ", PRCDDA, "| Censor Division Name: ", `CDname/DRnom`,
                                                         "| Population: ", `Dissemination area (DA) Population`,
                                                         "| Situational vulnerability Score: ", `Economic dependency Scores`),
                             color = ~pal_Sv(SVQ),
                             stroke = FALSE, fillOpacity = 0.8)%>%
            addLegend(pal=pal_Sv, values=mapdata()$SVQ,opacity=1, na.label = "Not Available")%>%
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="ME",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    
    
    output$hist <- renderPlot({
        par(mfrow=c(4,1))
        hist(mapdata()$`Residential instability Scores`, breaks = 30, col=sequential_hcl(1), xlab = "Scores", main = "Histogram of Residential Instability Scores")
        hist(mapdata()$`Economic dependency Scores`, breaks = 30, col=sequential_hcl(1 , h = 0, c = c(100, 0), l = 65, power = 1), xlab = "Scores", main = "Histogram of Economic Dependency Scores")
        hist(mapdata()$`Ethno-cultural composition Scores`, breaks = 30, col =sequential_hcl(1, h = 40, c = c(100, 0), l = 65, power = 1), xlab = "Scores", main = "Histogram of Ethno-cultural Composition Scores")
        hist(mapdata()$`Situational vulnerability Scores`, breaks = 30, col = sequential_hcl(1, h = 200, c = c(100, 0), l = 65, power = 1), xlab = "Scores", main = "Histogram of Situational Vulnerability Scores")
        
    })
    
    output$dataCDI <-DT::renderDataTable(datatable(
        CDI_lang_lat[,c(2:12, 19)],filter = 'top', class = 'cell-border stripe', 
        colnames = c("No." , "PRCDDA", "Province", "Dissemination area (DA) Population",
                     "Residential instability Quintiles", "Residential instability Scores", 
                     "Economic dependency Quintiles", "Economic dependency Scores", 
                     "Ethno-cultural composition Quintiles","Ethno-cultural composition Scores",   
                     "Situational vulnerability Quintiles", "Situational vulnerability Scores", "CDname/DRnom"
                     )
    ))
    
    
    
    geo <- list(
        showland = TRUE,
        showlakes = TRUE,
        showcountries = TRUE,
        showocean = TRUE,
        countrywidth = 0.5,
        landcolor = toRGB("limegreen"),
        lakecolor = toRGB("white"),
        oceancolor = toRGB("lightblue"),
        projection = list(
            type = 'orthographic',
            rotation = list(
                lon = -100,
                lat = 45,
                roll = 0
            )
        ),
        lonaxis = list(
            showgrid = TRUE,
            gridcolor = toRGB("gray40"),
            gridwidth = 0.8
        ),
        lataxis = list(
            showgrid = TRUE,
            gridcolor = toRGB("gray40"),
            gridwidth = 0.8
        )
    )
    
    
    
    output$Migration <- renderPlotly({
        plot_geo(color = I("red")) %>%
            add_markers(
                data = Location, x = ~Long, y = ~Lat, text = ~Location,
                size = 1, hoverinfo = "text", alpha = 0.5
            ) %>%
            add_segments(
                data = NetworkLoc_2_2,
                x = ~From.Long, xend = ~To.Long,
                y = ~From.Lat, yend = ~To.Lat,
                text= ~paste0("Case Name: ",CaseName," | Source: ", Source),
                alpha = 0.3, size = I(2), hoverinfo = "text"
            ) %>%
            layout(geo = geo, showlegend = TRUE)
        
    })
    
    output$dataTC <-DT::renderDataTable(datatable(
        NetworkLoc_2_2[,c(2:7, 12, 13)],filter = 'top', class = 'cell-border stripe', 
        colnames = c("No.", "CaseName", "From","To","Other Place Mentioned", "From.Prov", "To.Prov" , "Fact Summary:","Source"
        )
    ))
    
    

        
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
