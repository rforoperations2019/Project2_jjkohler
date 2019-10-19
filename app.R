library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)

bigfoot <- read.csv("bigfoot.csv", check.names=FALSE)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Bigfoot Sightings",
                          dropdownMenu(type = "messages",
                        notificationItem(text = "New Sasquatch sighting!", 
                        icon = icon("users"))
                 )
        )

    # Sidebar with a slider input for number of bins 
    # Dashboard Sidebar ----------------------------------------------
    sidebar <- dashboardSidebar(
        useShinyalert(),
        sidebarMenu(
            id = "tabs",
            
            # # Menu Items ----------------------------------------------
            menuItem("Bigfoot at a Glance", icon = icon("tree"), tabName = "intro"),
            menuItem("Bigfeet Plots", icon = icon("bar-chart"), tabName = "plot"),
            menuItem("Sasquatch Table", icon = icon("table"), tabName = "table"),

            #Year Selection ----------------------------------------------
            chooseSliderSkin("HTML5", color="SaddleBrown"),
            sliderInput("slider2", label = h4("Sighting Years"), min = 1920, 
                        max = 2018, value = c(1920, 2018), sep = ''),
            
            #Season Selection --------------------------------------------
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
                      placement = "bottom", trigger = "hover")
            )
    )



# Dashboard body ----------------------------------------------
body <- dashboardBody(
    tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css?family=Mansalva&display=swap');
      .main-header .logo {
        font-family: 'Mansalva', cursive;
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
                fluidRow(column(4,
                imageOutput("image"))

                ),
            
            fluidRow(
                valueBoxOutput("count")),
            fluidRow(
                infoBoxOutput("month")),
            fluidRow(
                valueBoxOutput("state"))
             
         )
    ),
    # Plot page ----------------------------------------------
    tabItem("plot",
            

            # Plots ----------------------------------------------
            fluidRow(
                box(plotlyOutput("plot_state"), width = 9)),
            fluidRow(
                box(plotlyOutput("plot_year"), width = '6'),
                box(plotlyOutput("plot_month"), width = '3')
                )
            ),
    
    # Data Table Page ----------------------------------------------
    tabItem("table",
            fluidPage(
                box(title = "Reported Bigfoot Sightings", DT::dataTableOutput("table"), width = 9))
        )
    )
)

ui <- dashboardPage(header, sidebar, body, skin='green')

# Define server function -----
server <- function(input, output) {

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
        
#Render Sasquatch image for landing page
        
    output$image <- renderImage({
            return(list(
                src = "sasquatch.jpg",
                contentType = 'image/jpeg',
                alt = "Is this Bigfoot?"
            ))
        }, deleteFile = FALSE)

#Filter data table from inputs--------------------------
    
    data <- reactive({
        #Check for existence of inputs --------------------------
        validate(
            need(input$class_button, "Select at least one Class"),
            need(input$season, "Select at least one Season")
        )
        sub <- subset(bigfoot, select = c('state','season','date','title','classification','month', 'year')) 
        colnames(sub) <- c('State', 'Season', 'Date', 'Description', 'Class', 'Month','Year')
        sub <- sub[(sub['Year'] >= input$slider2[1]) & (sub['Year'] <= input$slider2[2]),]
        sub <- subset(sub, Class %in% input$class_button)
        sub <- subset(sub, Season %in% input$season)
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