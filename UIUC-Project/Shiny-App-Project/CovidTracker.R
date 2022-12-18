#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(httr)
library(sqldf)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(zoo)
library(shinyWidgets)
library(leaflet)
library(DT)
library(png)
source("theme.R")
# Read API
base_url<-"https://api.covidtracking.com/"
info_url<-"v1/us/daily.json"
full_url<-base::paste0(base_url, info_url)
api_call <- httr::GET(full_url)
#api_call$status_code
data_raw <- jsonlite::fromJSON(base::rawToChar(api_call$content))

# Read States API
url<-"https://api.covidtracking.com/v1/states/daily.json"
api_call <- httr::GET(url)
api_call$status_code
data_raw1 <- jsonlite::fromJSON(base::rawToChar(api_call$content))
data_states<- sqldf::sqldf("
      SELECT date, state, positiveIncrease, hospitalizedIncrease, deathIncrease, inIcuCurrently
      FROM data_raw1
      ") %>%
  drop_na() %>%
  apply(2, rev) %>%
  as.data.frame()

#function to draw plot1
drawplot<-function(data, start_date, end_date, cg1){
  renderPlot(data[(7+start_date-as.numeric(as.Date("2020-04-01"))):(7+end_date-as.numeric(as.Date("2020-04-01"))),] %>%
               ggplot()+
               {if (1 %in% cg1)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=positive, colour="Positive"))
               }+
               {if (2 %in% cg1)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=negative, colour="Negative"))
               }+
               scale_y_continuous(labels=scales::comma)+
               labs(title = "Trend of Cases",
                    x="Date",
                    y="Number")+
               theme(plot.title=element_text(size=28, face="bold", hjust=0.5, vjust=-3),
                     axis.text=element_text(size=14),
                     axis.title = element_text(size = 20),
                     legend.text = element_text(size = 20),
                     panel.background=element_blank(), panel.grid=element_line(color="gray", linetype=3))+
               scale_colour_manual("", breaks=c("Positive", "Negative"),
                                   values=c("red", "blue"))
  )}

#function to draw plot2
drawplot2<-function(data, start_date, end_date, cg2, cg3){
  renderPlot(data[(7+start_date-as.numeric(as.Date("2020-04-01"))):(7+end_date-as.numeric(as.Date("2020-04-01"))),] %>%
               ggplot()+
               {if (3 %in% cg2)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=hospitalizedCurrently, colour="Hospitalized Currently"))
               }+
               {if (4 %in% cg2)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=hospitalizedIncrease, colour="Hospitalized Increase"))
               }+
               {if (5 %in% cg2)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=inIcuCurrently, colour="In ICU Currently"))
               }+
               {if (6 %in% cg2)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=onVentilatorCurrently, colour="On Ventilator Currently"))
               }+
               {if (7 %in% cg3)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=deathIncrease, colour="deathIncrease"))
               }+
               {if (8 %in% cg3)
                 geom_line(mapping=aes(x=as.Date(as.character(date), "%Y%m%d"),y=death, colour="death"))
               }+
               scale_y_continuous(labels=scales::comma)+
               labs(title = "Trend of Cases",
                    x="Date",
                    y="Number")+
               theme(plot.title=element_text(size=28, face="bold", hjust=0.5, vjust=-3),
                     axis.text=element_text(size=14),
                     axis.title = element_text(size = 20),
                     legend.text = element_text(size = 20),
                     panel.background=element_blank(), panel.grid=element_line(color="gray", linetype=3))+
               scale_colour_manual("", breaks=c("Hospitalized Currently", "Hospitalized Increase", "In ICU Currently", "On Ventilator Currently", "deathIncrease", "death"),
                                   values=c("#009E73","#E69F00","#999999","#56B4E9","#CC79A7","#F0E443"))
  )}

#function to show table
showtable<-function(data, start_date, end_date, cg4){
  date_interval <- (80+start_date-as.numeric(as.Date("2020-04-01"))):(80+end_date-as.numeric(as.Date("2020-04-01")))
  datachoose <- data[date_interval,]
  row.names(datachoose) <- NULL
  mindate <-function(case){
    paste(substr(datachoose[which.min(datachoose[,case]),"date"],1,4),
          substr(datachoose[which.min(datachoose[,case]),"date"],5,6),
          substr(datachoose[which.min(datachoose[,case]),"date"],7,8),sep="-")
  }
  maxdate <-function(case){
    paste(substr(datachoose[which.max(datachoose[,case]),"date"],1,4),
          substr(datachoose[which.max(datachoose[,case]),"date"],5,6),
          substr(datachoose[which.max(datachoose[,case]),"date"],7,8),sep="-")
  }
  generatetext <- function(casetype){
    c(mindate(casetype),
      min(datachoose[,casetype]),
      maxdate(casetype),
      max(datachoose[,casetype]),
      round(mean(datachoose[,casetype]),0),
      sum(datachoose[,casetype]))
  }
  row_pi <- generatetext("positiveIncrease")
  row_hi <- generatetext("hospitalizedIncrease")
  row_di <- generatetext("deathIncrease")
  data_a <- data.frame("Positive Increase"=row_pi, "Hospitalized Increase"=row_hi, "Death Increase"=row_di)
  rownames(data_a) <- c("Date of Minimum Cases","Minimum Cases per Day","Date of Maximum Cases","Maximum Cases per Day","Average Cases per Day","Total Cases")
  data_show<-data.frame(matrix(ncol = 0, nrow = dim(data_a)[1]))
  rownames(data_show) <- c("Date of Minimum Cases","Minimum Cases per Day","Date of Maximum Cases","Maximum Cases per Day","Average Cases per Day","Total Cases")
  if (1 %in% cg4){
    data_show<-cbind(data_show, "Positive Increase"=data_a[,"Positive.Increase"])}
  if (2 %in% cg4){
    data_show<-cbind(data_show,"Hospitalized Increase"=data_a[,"Hospitalized.Increase"])}
  if (3 %in% cg4){
    data_show<-cbind(data_show,"Death Increase"=data_a[,"Death.Increase"])}
  DT::renderDataTable(data_show
                      #data_show<-data[(7+start_date-as.numeric(as.Date("2020-04-01"))):(7+end_date-as.numeric(as.Date("2020-04-01"))),])
  ) 
}
#function of state's statement
stateinfo <- as.data.frame(cbind(name=state.name,abb=state.abb,x=state.center$x,y=state.center$y))
showinfo <- function(state_name, select_date){
  date_select <- paste(str_split(select_date,"-")[[1]], collapse = '')
  if (identical(character(0),data_states[data_states$date==date_select&data_states$state==stateinfo[stateinfo$name==state_name,"abb"],"positiveIncrease"])==TRUE){
    result <- paste0(state_name,"<br><br>","NA")}
  else{
    result <- paste0(state_name,"<br><br>",
                     "Positive Increase: ",data_states[data_states$date==date_select&data_states$state==stateinfo[stateinfo$name==state_name,"abb"],"positiveIncrease"],"<br><br>",
                     "Hospitalized Increase: ",data_states[data_states$date==date_select&data_states$state==stateinfo[stateinfo$name==state_name,"abb"],"hospitalizedIncrease"],"<br><br>",
                     "Death Increase: ",data_states[data_states$date==date_select&data_states$state==stateinfo[stateinfo$name==state_name,"abb"],"deathIncrease"])}
  return(result)
}
#function to show map
showmap <- function(input_date, input_state){
  awesome <- makeAwesomeIcon(
    icon = "fa-thin fa-flag",
    iconColor = "#FFFFFF",
    markerColor = "blue",
    library = "fa"
  )
  renderLeaflet({
    leaflet() %>% addTiles() %>% setView(-93.85, 37.45, zoom = 5) %>% 
      setMaxBounds(lng1 = -70
                   , lat1 = 25
                   , lng2 = -130
                   , lat2 = 49) %>%
      addAwesomeMarkers(icon = awesome,lng=as.numeric(stateinfo[stateinfo$name==input_state,"x"]), lat=as.numeric(stateinfo[stateinfo$name==input_state,"y"]), popup=showinfo(input_state, input_date))
    #addLegend("bottomright", colors= c("#ffa500"), labels=c("States"), title="States")
  })
}
# options for states
vars <- c(
  "Alabama" = "Alabama",
  "Arizona" = "Arizona",
  "Arkansas" = "Arkansas",
  "California" = "California",
  "Colorado" ="Colorado",
  "Connecticut"="Connecticut",
  "Delaware"="Delaware",
  "Florida"="Florida",
  "Georgia"="Georgia",
  "Hawaii"="Hawaii",
  "Idaho"="Idaho",
  "Illinois"="Illinois",
  "Indiana"="Indiana",
  "Iowa"="Iowa",
  "Kansas"="Kansas",
  "Kentucky"="Kentucky",
  "Louisiana"="Louisiana",
  "Maine"="Maine",
  "Maryland"="Maryland",
  "Massachusetts"="Massachusetts",
  "Michigan"="Michigan",
  "Minnesota"="Minnesota",
  "Mississippi"="Mississippi",
  "Missouri"="Missouri",
  "Montana"="Montana",
  "Nebraska"="Nebraska",
  "Nevada"="Nevada",
  "New Hampshire"="New Hampshire",
  "New Jersey"="New Jersey",
  "New Mexico"="New Mexico",
  "New York"="New York",
  "North Carolina"="North Carolina",
  "North Dakota"="North Dakota",
  "Ohio"="Ohio",
  "Oklahoma"="Oklahoma",
  "Oregon"="Oregon",
  "Pennsylvania"="Pennsylvania",
  "Rhode Island"="Rhode Island",
  "South Carolina"="South Carolina",
  "South Dakota"="South Dakota",
  "Tennessee"="Tennessee",
  "Texas"="Texas",
  "Utah"="Utah",
  "Vermont"="Vermont",
  "Virginia"="Virginia",
  "Washington"="Washington",
  "West Virginia"="West Virginia",
  "Wisconsin"="Wisconsin",
  "Wyoming"="Wyoming"
)

#ui
ui <- dashboardPage(
  dashboardHeader(title = span("Covid-19 Tracker", style="color:white; font-weight: bold; font-size: 23px")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("glyphicon glyphicon-info-sign", lib="glyphicon")),
      menuItem("Chart", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Table", tabName = "Table", icon = icon("glyphicon glyphicon-th-list", lib="glyphicon")),
      menuItem("Map", tabName = "Map", icon = icon("glyphicon glyphicon-map-marker", lib="glyphicon"))
    )
  ),
  dashboardBody(
    custom_theme,
    tabItems(
      # First tab content
      tabItem(tabName = "Introduction",
              fluidRow(
                column(width=5,
                       box(
                         title = span("About this Dashboard", style="font-style: italic; font-weight: bold; font-size: 23px"),  width = NULL, solidHeader = FALSE, status = "primary",
                         h4("The data set is requested from the API: https://api.covidtracking.com. It is collected from state/district/territory public health authorities or, occasionally, from trusted news reporting, official press conferences,
                  or (very occasionally) tweets or Facebook updates from state public health authorities or governors. It contains information from 50 US states, the District of Columbia, 
                 and 5 other US territories to provide the most comprehensive testing data for the novel coronavirus. The data collection period started from April 1, 2020 and ended as of March 7, 2021."),
                         h4("In this dashboard, we include the following fields:"),
                         span("date, positive, positiveIncrease, death, negative, hospitalizedCurrently, hospitalizedIncrease, inIcuCurrently, onVentilatorCurrently, death, deathIncrease.", style="font-style: italic;font-size: 16px")
                       ),
                       #tags$img(src="covid.png")
                       #box(title = "CovidPic",width=7, imageOutput("Covid"))
                       box(
                         title = span("Tabs", style="font-style: italic; font-weight: bold; font-size: 23px"),  width = NULL, solidHeader = FALSE, status = "primary",
                         h4("There are three other tabs in this dashboard:"),
                         span("1. Chart", style="font-style: italic; font-size: 18px"),
                         h4("The chart tab includes 2 tabs, each generating a plot showing the trend of the categories selected within the period of dates selected."),
                         span("2. Table", style="font-style: italic; font-size: 18px"),
                         h4("The table tab generates a table showing the summary statistics of the data within the period of dates selected."),
                         span("3. Map", style="font-style: italic; font-size: 18px"),
                         h4("The map tab shows the number of increased positive cases, increased hospitalized cases, and increased death cases by specific dates and states on a map of the United States.")
                       )
                ),
                column(width=7,
                       box(title = NULL, solidHeader = FALSE, width=NULL, imageOutput("Covid", height="auto")
                       )
                )
              )    
      ),
      #Second tab content
      tabItem(tabName = "Dashboard",
              fluidRow(
                setSliderColor(c("#a5b8b8","#a5b8b8","#a5b8b8"), c(1,2,3)),
                tabBox(
                  #title = "First tabBox",
                  id = "tabset1", height = "750px", width = "300px",
                  tabPanel(span("Positive & Negative", style="font-weight: bold; font-size: 18px"), 
                           column(width=5,
                                  box(
                                    title = span("Test Result", style="font-weight: bold; font-size: 18px"), width = NULL, solidHeader = TRUE, status = "primary",
                                    checkboxGroupInput("checkGroup1", label = NULL, choices = list("Positive Cases" = 1, "Negative Cases"=2), 
                                                       selected=c(1,2))
                                  ),
                                  box(width = NULL,sliderInput("obs", "Plotting Range of Dates:",
                                                               min = as.Date("2020-04-01","%Y-%m-%d"), 
                                                               max = as.Date("2021-03-07","%Y-%m-%d"), 
                                                               value = as.Date(c("2020-05-01","2020-08-01")),timeFormat="%Y-%m-%d")
                                  ),
                                  actionButton(inputId ="button1",label=span("Submit", style="font-size: 15px"))
                          ),column(width=7,
                                   box(width = NULL,plotOutput("plot1", height = 580))
                          )
                          ),
                  tabPanel(span("Hospitalized & Death", style="font-weight: bold; font-size: 18px"), 
                           column(width=5,
                                  box(
                                    title = span("Hospitalized", style="font-weight: bold; font-size: 18px"), width = NULL, solidHeader = TRUE, status = "warning",
                                    checkboxGroupInput("checkGroup2", label = NULL, 
                                                       choices = list("Currently Hospitalized" =3, 
                                                                      "Daily Increase of Hospitalized"=4,
                                                                      "Currently Hospitalized in the Intensive Care Unit (ICU)"=5,
                                                                      "Currently Hospitalized Under Advanced Ventilation"=6),
                                                       selected=c(3,5))
                                  ),
                                  box(
                                    title = span("Death", style="font-weight: bold; font-size: 18px"), width = NULL, solidHeader = TRUE, status = "danger",
                                    checkboxGroupInput("checkGroup3", label = NULL, 
                                                       choices = list("Daily Increase in Death" =7, 
                                                                      "Cummulative Deaths"=8),
                                                       selected=c(7))
                                  ),
                                  box(width = NULL,sliderInput("obs2", "Plotting Range of Dates:",
                                                               min = as.Date("2020-04-01","%Y-%m-%d"), 
                                                               max = as.Date("2021-03-07","%Y-%m-%d"), 
                                                               value = as.Date(c("2020-05-01","2020-08-01")),timeFormat="%Y-%m-%d")
                                  ),
                                  tags$head(
                                    tags$style(HTML("
                                  .btn {
                                    display:block;
                                    height: 40px;
                                    width: 100px;
                                    border-radius:5%;
                                    border: 2px rgb(165, 184, 184);
                                    }
                                    "))
                                  ),
                                  actionButton(inputId ="button2",label=span("Submit", style="font-size: 15px"))
                           ),
                           column(width=7,
                                  box(width = NULL,plotOutput("plot2", height = 580))
                           )
                                  )
                )
              )
      ),
      # Third tab content
      tabItem(tabName = "Table",
              fluidRow(
                column(width=5,
                       box(
                         title = span("Select Categories", style="font-weight: bold; font-size: 18px"), width = NULL, solidHeader = TRUE, status = "danger", 
                         checkboxGroupInput("checkGroup4", label = NULL, 
                                            choices = list("Positive Cases" = 1, 
                                                           "Daily Increase of Hospitalized"=2,
                                                           "Daily Increase in Death" =3), 
                                            selected=c(1, 2))
                       ),
                       box(width = NULL,sliderInput("obs1", "Select Observing Range of Dates:",
                                                    min = as.Date("2020-04-01","%Y-%m-%d"), 
                                                    max = as.Date("2021-03-07","%Y-%m-%d"), 
                                                    value = as.Date(c("2020-05-01","2020-08-01")),timeFormat="%Y-%m-%d")
                       ),
                       actionButton(inputId ="button3",label=span("Submit", style="font-size: 15px"))
                ),
                column(width=7,
                       box(width = NULL, dataTableOutput("table1")
                       )
                )
              )
      ),
      # Fourth tab content
      tabItem(tabName = "Map",
              tags$head(
                # Include our custom CSS
                includeCSS("style1.css")
              ),
              tags$style(type="text/css","#myMap {height: calc(100vh - 80px) !important;}"),
              leafletOutput('myMap'),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = "auto", left = 350, right = "auto", bottom = "100",
                            width = 370, height = 270, h4("Click the marker to see the information."),
                            dateInput("date", label = h5("Select Date:"), 
                                      min = "2020-04-01",
                                      max = "2021-03-07",
                                      format = "yyyy-mm-dd",
                                      value = "2020-05-30"),
                            selectInput("state", h5("Select State:"), vars, selected = "California"),
                            actionButton(inputId ="button4",label=span("Submit", style="font-size: 15px"))
              )
              
      )
    )
  )
)

server <- function(input, output) {
  output$Covid <- renderImage({
    return(list(src = "covid2.jpeg",
         contentType = "image/jpeg",
         width = "100%",
         height = 662))
  }, deleteFile = FALSE)
  data1<- sqldf::sqldf("
      SELECT date, positive, positiveIncrease, pending, negative, hospitalizedCurrently, hospitalizedIncrease, inIcuCurrently, onVentilatorCurrently, death, deathIncrease
      FROM data_raw
      ") %>%
    drop_na() %>%
    apply(2, rev) %>%
    as.data.frame()
  output$plot1 <- drawplot(data1, input$obs[1], input$obs[2], input$checkGroup1)
  observeEvent(input$button1,{
    output$plot1 <- drawplot(data1, input$obs[1], input$obs[2], input$checkGroup1)
  })
  output$plot2 <- drawplot2(data1, input$obs2[1], input$obs2[2], input$checkGroup2, input$checkGroup3)
  observeEvent(input$button2,{
    output$plot2 <- drawplot2(data1, input$obs2[1], input$obs2[2], input$checkGroup2, input$checkGroup3)
  })
  data2<- sqldf::sqldf("
      SELECT date, positiveIncrease, hospitalizedIncrease, deathIncrease
      FROM data_raw
      ") %>%
    drop_na() %>%
    apply(2, rev) %>%
    as.data.frame()
  output$table1 <- showtable(data2, 18383, 18475, c(1,2))
  observeEvent(input$button3,{
    output$table1 <- showtable(data2, input$obs1[1], input$obs1[2], input$checkGroup4)
  })
  
  output$myMap <- showmap("20200530","California")
  observeEvent(input$button4,{
    output$myMap <- showmap(input$date, input$state)
  })
}

shinyApp(ui, server)

