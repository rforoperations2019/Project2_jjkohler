library(shiny)
library(shinydashboard)
library(devtools)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(htmltools)


# Pulling from an API ---------------------- 
# Because APIs are COOL ---------------------------------
# a_poll <- readOGR("https://services1.arcgis.com/vdNDkVykv9vEWFX4/ArcGIS/rest/services/Allegheny_County_Polling_Places_May2019/FeatureServer/0/query?where=OBJECTID_1+%3E+0&objectIds=&time=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=")

a_data <- readOGR('VTDs_Oct17.shp')


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Allegheny 2016"
                          
)

# Sidebar with a slider input for number of bins 
# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
    useShinyalert(),
    sidebarMenu(
        id = "tabs",
        
        # # Menu Items ----------------------------------------------
        menuItem("County Map", icon = icon("tree"), tabName = "intro"),
        menuItem("Data Table", icon = icon("table"), tabName = "table"),
        

        #Map Layer Selection --------------------------------------------
        shiny::actionButton( inputId = "election"
                             , icon = icon( name = "fa-vote-yea")
                             , label = " 2016 Votes "
                             , style = "color: #fff; background-color: #D75453; border-color: #C73232"
        ),
        shiny::actionButton( inputId = "white"
                             , icon = icon( name = "fa-vote-yea")
                             , label = "Percent White"
                             , style = "color: #fff; background-color: #D75453; border-color: #C73232"
        ),
        shiny::actionButton( inputId = "black"
                             , icon = icon( name = "fa-vote-yea")
                             , label = "Percent Black"
                             , style = "color: #fff; background-color: #D75453; border-color: #C73232"
        ),
        shiny::actionButton( inputId = "clear"
                             , icon = icon( name = "eraser")
                             , label = "Clear the Map"
                             , style = "color: #fff; background-color: #D75453; border-color: #C73232"
        ),
        
        prettyCheckboxGroup(inputId='season', label= h4("Season of Sighting"), choices = c('Spring','Summer','Fall','Winter'), 
                            selected = c('Spring','Summer','Fall','Winter'),
                            status = "default", shape = "curve",
                            outline = FALSE, fill = FALSE, thick = TRUE, animation = 'pulse',
                            icon = NULL, plain = FALSE, bigger = FALSE, inline = FALSE,
                            width = NULL, choiceNames = NULL, choiceValues = NULL),
        
        #Class Selection --------------------------------------------
        checkboxGroupButtons(
            inputId = "class_button", label = h4("Choose Sighting Class :"), 
            choices = c("Class A", "Class B"),
            selected = c("Class A", "Class B"),
            justified = TRUE, status = "primary",
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                             no = icon("remove", lib = "glyphicon"))
        ),
        
        #Class Tooltip ----------------
        bsTooltip(id = "class_button", title = "<strong>Class A</strong><br> <i>Reports of clear sightings.</i><br><br> <strong>Class B</strong><br> <i>Observations without a clear view.</i>",
                  placement = "bottom", trigger = "hover"),
        
        downloadButton("downloadData", "Download Data")
    
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
                    width = 10
                    , leaflet::leafletOutput( outputId = "map"
                                              , height = 700,
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Allegheny16-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(a_data, file)
    }
  )
    
    #Error if no Season is chosen
    observeEvent(input$season, ignoreNULL = FALSE,{
        if (length(input$season)==0) {
            shinyalert("Oh No!", "The Sasquatch may just hibernate or go south if you don't choose a season.", type = "warning")
        }
    }
    )
    
    #Error if no Class is chosen 
    observeEvent(input$class_button, ignoreNULL = FALSE,{
        if (length(input$class_button)==0) {
            shinyalert("Oops!", "You won't have any sightings if you don't pick at least one class.", type = "error")
        }
    }
    )
    

    # create foundational map

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
    dempal <- colorFactor(c("#2aa1ec", "#fe6a59"), a_data@data$WIN)
# HTML Labels: https://stackoverflow.com/questions/43144596/r-and-leaflet-how-to-arrange-label-text-across-multiple-lines
    
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
                                                   bringToFront = TRUE)
               # , layerId = unique(a_data@data$OBJECTID_1)
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
                                                     bringToFront = TRUE)
                 # , layerId = unique(a_data@data$OBJECTID_1)
                 # , group = "click.list"
    )
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
                                                     bringToFront = TRUE)
                 # , layerId = unique(a_data@data$OBJECTID_1)
                 # , group = "click.list"
    )
})    

observeEvent(input$clear, {
  leafletProxy('map') %>% clearShapes() 
    
})    

    # observe({
    #   pal <- colorpal()
    #   
    #   leafletProxy("map", data = filteredData()) %>%
    #     clearShapes() %>%
    #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
    #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
    #     )
    # })
    
    #Filter data table from inputs--------------------------
    
    data <- reactive({
        #Check for existence of inputs --------------------------
        # validate(
        #     need(input$class_button, "Select at least one Class"),
        #     need(input$season, "Select at least one Season")
        # )
        sub <- subset(a_data@data, select = c("COUNTYNAME","MCD_NAME", "VTD_NAME", "VAPERSONS", "T16PRESD","T16PRESR", "T16PRESOTH", "X.DEM",      "X.REP","X.OTH","X.WHITE","X.BLACK","X.ORACE"  )) 
        colnames(sub) <- c("County","Municipality","Ward/Dist","Voting Age","Dem","Rep","Other","% Dem","% Rep","% Oth","% White","% Black",    "% Oth Race"   )
        # sub <- sub[(sub['Year'] >= input$slider2[1]) & (sub['Year'] <= input$slider2[2]),]
        # sub <- subset(sub, Class %in% input$class_button)
        # sub <- subset(sub, Season %in% input$season)
        sub
        
    }
    )
    
    # Aggregating sightings by state for plotting --------------------------
    state_count <- reactive({count <- aggregate(x = data()[c('State','Year')],
                                                by = list(states = data()$State),FUN = length)
    colnames(count) <- c('State', 'Count','Year')
    count <- count[order(count$Year),]
    count
    
    })
    
    # Aggregating sightings by year for plotting --------------------------
    year_count <- reactive({count <- aggregate(x = data()[c('State','Year')],
                                               by = list(years = data()$Year),FUN = length)
    colnames(count) <- c('Year', 'Count','State')
    count <- count[order(count$Year),]
    count
    
    })
    
    # Aggregating sightings by month for plotting --------------------------
    month_count <- reactive({count <- aggregate(x = data()[c('State','Month')],
                                                by = list(months = data()$Month),FUN = length)
    colnames(count) <- c('Month1','Count','State')
    count <- count[order(count$Month1),]
    count$Month <- month.abb[count$Month1]
    count
    
    })
    
    # Aggregating sightings by month for sorted by value --------------------------
    month_max <- reactive({count <- aggregate(x = data()[c('State','Month')],
                                              by = list(months = data()$Month),FUN = length)
    colnames(count) <- c('Month1','Count','State')
    count <- count[order(count$Count),]
    count$Month <- month.name[count$Month1]
    count
    
    })
    
    # Data table of Bigfoot Sightings ----------------------------------------------
    output$table <- DT::renderDataTable({
        data()
    })
    
    # A plot showing sightings by state -----------------------------    
    output$plot_state <- renderPlotly({
        ggplotly(  
            p1 <- ggplot(state_count(), aes(x = reorder(State, -Count), y = Count, label=State)) +
                geom_bar(stat = "identity", width = 0.8, color='darkgreen', fill='forestgreen')+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle('Bigfoot Sightings By State')+
                xlab("State") + 
                ylab("Count of Sightings"), tooltip=c('label','y')
        )
    })
    
    # A plot showing sightings by year -----------------------------    
    output$plot_year <- renderPlotly({
        ggplotly(  
            p2 <- ggplot(year_count(), aes(x = Year, y = Count, label=Year)) +
                geom_line(color='forestgreen') + geom_point(color='saddlebrown', stroke='forestgreen') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle('Yearly Sasquatches')+
                xlab("Year") + 
                ylab("Count of Sightings"), tooltip=c('label','y')
        )
    })
    
    # A plot showing sightings by month -----------------------------    
    output$plot_month<- renderPlotly({
        ggplotly(  
            p3 <- ggplot(month_count(), aes(x = reorder(Month, -Month1), y = Count, label=Month)) +
                geom_bar(stat = "identity", width = 0.8, color='darkgreen', fill='saddlebrown')+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle("Bigfoot's Favorite Month")+
                xlab("Month") + 
                ylab("Count of Sightings") +
                coord_flip(), tooltip=c('label','y') 
        )
    })
    
    
    # Value Boxes ----------------------------------------------
    output$count <- renderValueBox({
        val <- length(data()$Date)
        valueBox(subtitle = "Total Sightings", value = val, icon = icon("tree"), color = "green")
    })
    
    output$month <- renderInfoBox({
        val <- tail(month_max()$State, n=1)
        mn <- tail(month_max()$Month, n=1)
        
        infoBox("Peak Month", value = mn, subtitle = paste(val, " Sightings", sep = ''), icon = icon("calendar-alt"), color = "olive")
    })    
    
    output$state <- renderValueBox({
        st <- tail(state_count()$State, n=1)
        val <- tail(state_count()$Count, n=1)
        valueBox(subtitle = paste("Top State with", val,'Sightings'), value = st, icon = icon("flag"), color = "green")
    })
    

    
}
# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)