library(shiny)
library(leaflet)
library(shinydashboard)
library(devtools)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(proj4)
library(htmltools)
library(rgdal)


# Pulling from an API ---------------------- 
# Because APIs are COOL ---------------------------------
a_poll <- readOGR("https://services1.arcgis.com/vdNDkVykv9vEWFX4/ArcGIS/rest/services/Allegheny_County_Polling_Places_May2019/FeatureServer/0/query?where=OBJECTID_1+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=")

xy <- a_poll@data %>% select(POINT_X,POINT_Y,OBJECTID_1)
#assign projection
proj4string <- "+proj=lcc +lat_1=40.96666666666667 +lat_2=39.93333333333333 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
#project point data
pj <- proj4::project(xy, proj4string, inverse=TRUE)
#create latlon
points <- data.frame(xy, lat=pj$y, lon=pj$x)
#merge
final <-  merge(a_poll@data, points, by.x = "OBJECTID_1", by.y = "OBJECTID_1")


a_data <- readOGR('VTDs_Oct17.shp')


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Allegheny 2016"
                          
)

# Sidebar with a slider input for number of bins 
# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        # # Menu Items ----------------------------------------------
        menuItem("County Map", icon = icon("tree"), tabName = "intro"),
        menuItem("Data Table", icon = icon("table"), tabName = "table"),
        

        #Map Layer Selection Buttons --------------------------------------------

        actionBttn(inputId = "election", label = " 2016 Voting ", icon = NULL, style = "unite",
                    color = "danger", size = "sm", block = FALSE, no_outline = TRUE),

        actionBttn(inputId = "white", label = "Percent White", icon = NULL, style = "unite",
                   color = "danger", size = "sm", block = FALSE, no_outline = TRUE),
        
        actionBttn(inputId = "black", label = "Percent Black", icon = NULL, style = "unite",
                   color = "danger", size = "sm", block = FALSE, no_outline = TRUE),
        
        actionBttn(inputId = "polls", label = "Poll Locations", icon = NULL, style = "unite",
                   color = "danger", size = "sm", block = FALSE, no_outline = TRUE),
        
        actionBttn(inputId = "clear", label = "Clear Layers", icon = NULL, style = "unite",
                   color = "danger", size = "sm", block = FALSE, no_outline = TRUE),
        
        downloadButton("downloadData", "Download")
    
    )
)



# Dashboard body ----------------------------------------------
body <- dashboardBody(
    tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css?family=Arvo&display=swap');
      .main-header .logo {
        font-family: 'Arvo', serif;
        font-weight: bold;
        font-size: 24px;
      }
          .small-box {height: 100px}
          .info-box {min-height: 100px;}
          .info-box-icon {height: 100px; line-height: 100x;} 
          .info-box-content {padding-top: 5px; padding-bottom: 0px;}
    ")))
    # https://stackoverflow.com/questions/35422946/r-shinydashboard-change-height-of-valuebox
    # https://stackoverflow.com/questions/37861234/adjust-the-height-of-infobox-in-shiny-dashboard
    ,
    tabItems(
        # Intro page ----------------------------------------------
        tabItem("intro",
                fluidPage(
                  column(
                    width = 8,
                    fluidRow(
                    leaflet::leafletOutput( outputId = "map"
                                              , height = 800
                    ),
                    fluidRow(h5("Please Be Patient. App response time is very slow. After clicking on a layer button don't click twice - wait for original selection to load before proceeding. Click on the loaded map layers to activate plots."))
                  )),
                  column(width = 4,
                    fluidRow(
                      box(plotlyOutput("plot_race"), width = 12)
                    ),
                    fluidRow(
                      box(plotlyOutput("plot_voting"), width = 12)
                    )
                  )
                ) # end of the box
        
        ),
# Data Table Page ----------------------------------------------
        tabItem("table",
                fluidPage(
                    box(title = "Allegheny County 2016 Statistics", DT::dataTableOutput("table"), width = 9))
        )
    )
)

ui <- dashboardPage(header, sidebar, body, skin='blue')

# Define server function -----

server <- function(input, output) {
# Allow Data Download ------------------------------------------------  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Allegheny16-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
    

# create foundational map------------------------------

    foundational.map <- shiny::reactive({
      leaflet(options = leafletOptions(zoomSnap=0.1)) %>%
        addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
        setView( lng = -79.9959
                 , lat = 40.4406
                 , zoom = 11 ) 
    })
    
    output$map <- renderLeaflet({
      
      foundational.map()
      
    }) # end of leaflet::renderLeaflet({})

    
# Reference: rpubs.com/bhaskarvk/electoral-Map-2016
# Set Color Pallete for Votting Map --------------------------------
    
    dempal <- colorFactor(c("#2aa1ec", "#fe6a59"), a_data@data$WIN)
# HTML Labels: https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
    
#Define Hover Labels ---------------------------------------------------------
    
labs <- lapply(seq(nrow(a_data@data)), function(i) {
      paste0("<strong>", a_data@data[i, "MCD_NAME"],"</strong><br>",
             a_data@data[i, "VTD_NAME"],"<br><br>",
             "<strong>Dem: </strong>", a_data@data[i, "X.DEM"]*100, "% <br>",
             "<strong>Rep: </strong>", a_data@data[i, "X.REP"]*100, "% <br>",
             "<strong>Oth: </strong>", a_data@data[i, "X.OTH"]*100, "% <br>",
             "<br><strong>White: </strong>", a_data@data[i, "X.WHITE"]*100, "% <br>",
             "<strong>Black: </strong>", a_data@data[i, "X.BLACK"]*100, "% <br>",
             sep='') 
    })
    
#Button Inputs -------------------------------------------
    
observeEvent(input$election, {
  leafletProxy('map') %>% clearShapes() %>%
  addPolygons( data = a_data,
               fillColor= ~dempal(WIN),
               fillOpacity = ~ifelse(WIN=='D',(X.DEM),
                                     (X.REP))
               , opacity = ~ifelse(WIN=='D',(X.DEM)-0.2,
                                   (X.REP)-0.2),
               weight = 2,
               color = ~dempal(WIN),
               label= lapply(labs, htmltools::HTML),
               highlightOptions = highlightOptions(weight = 5,
                                                   color = "white",
                                                   fillOpacity = 0.7,
                                                   bringToFront = FALSE)
               , layerId = unique(a_data@data$OBJECTID_1)
               # , group = "click.list"
  )
})    

whitepal <- colorNumeric(
  c('#383632','#ffa200'),
  domain = c(0,1))

observeEvent(input$white, {
  leafletProxy('map') %>% clearShapes() %>%
    addPolygons( data = a_data,
                 fillColor= ~whitepal(X.WHITE),
                 fillOpacity = ~X.WHITE
                 , opacity = ~X.WHITE,
                 weight = 2,
                 color = "gray",
                 label= lapply(labs, htmltools::HTML),
                 highlightOptions = highlightOptions(weight = 5,
                                                     color = "white",
                                                     fillOpacity = 0.7,
                                                     bringToFront = FALSE
                 , layerId = unique(a_data@data$OBJECTID_1)
    ))
})
      

blackpal <- colorNumeric(
  c('#383632','#018023'),
  domain = c(0,1))

observeEvent(input$black, {
  leafletProxy('map') %>% clearShapes() %>%
    addPolygons( data = a_data,
                 fillColor= ~blackpal(X.BLACK),
                 fillOpacity = ~X.BLACK
                 , opacity = ~X.BLACK,
                 weight = 2,
                 color = "gray",
                 label= lapply(labs, htmltools::HTML),
                 highlightOptions = highlightOptions(weight = 5,
                                                     color = "white",
                                                     fillOpacity = 0.7,
                                                     bringToFront = FALSE)
                 , layerId = unique(a_data@data$OBJECTID_1)
                 )
})    

observeEvent(input$polls, {
  leafletProxy('map') %>% 
    addCircleMarkers(data=final,
      radius = 3,
      color = 'darkred',
      stroke = FALSE, fillOpacity = 0.6, label= ~LocName
    )
  
})    

observeEvent(input$clear, {
  leafletProxy('map') %>% clearShapes() %>% clearMarkers()

})

#Observe Map Clicks and Generate Plots -----------------------------

observe({
  validate(
    need(input$map_shape_click, "Click on the Map for more Info"))
  event <- input$map_shape_click

  # Filtering and plotting
  x <- a_data@data[a_data@data$OBJECTID_1 == event$id, ]
  
  #Race Bar Chart
  output$plot_race <- renderPlotly({
    p <- plot_ly(
      x = c("% White", "% Black", "% Other Race"),
      y = c(x$X.WHITE, x$X.BLACK,x$X.ORACE),
      name = "Voting Ward Race",
      type = "bar",
      marker = list(color = c('#ffa200', '#018023', '#80017a'))
    )%>% layout(
      title = paste(x$MCD_NAME, x$VTD_NAME, 'Race'),
         xaxis = list(title = "Race"),
        yaxis = list(title = "Percent"))
      
    
  })
  
  #Voting Bar Chart
  output$plot_voting <- renderPlotly({
    p1 <- plot_ly(
      x = c("% Dem", "% Rep", "% Other"),
      y = c(x$X.DEM, x$X.REP,x$X.OTH),
      name = "Voting Ward Percent",
      type = "bar",
      marker = list(color = c("#2aa1ec", "#fe6a59", '#14b83a'))
    )%>% layout(
      title = paste(x$MCD_NAME, x$VTD_NAME, 'Voting'),
      xaxis = list(title = "Party"),
      yaxis = list(title = "Percent"))
    
  })
})

#Generate Simplified Data Table --------------------------------    
    data <- reactive({
        sub <- subset(a_data@data, select = c("COUNTYNAME","MCD_NAME", "VTD_NAME", "VAPERSONS", "T16PRESD","T16PRESR", "T16PRESOTH", "X.DEM",      "X.REP","X.OTH","X.WHITE","X.BLACK","X.ORACE"  )) 
        colnames(sub) <- c("County","Municipality","Ward/Dist","Voting Age","Dem","Rep","Other","% Dem","% Rep","% Oth","% White","% Black",    "% Oth Race"   )
        sub
        
    }
    )
    

    
# Data table of Allegheny County Data ----------------------------------------------
    output$table <- DT::renderDataTable({
        data()
    })
    

    
}
# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)