#setting the shiny app environment
library(rsconnect)

rsconnect::setAccountInfo(name='zhuohao',
                          token='15789238E9BB10E18039583392933E45',
                          secret='Mi6QMCfVsQ7rxXhMnYj/7isdK9Kq1gxEoraXowgZ')

library(jsonlite)
library(httr)
library(lubridate)
library(dplyr)
library(shiny)

# #setting up the GET request url varibales
# url1 = 'https://api.tfl.gov.uk/Line/'
# line = 'Metropolitan'
# stopPointId = '940GZZLUWYP'
# url2 = '/Arrivals?stopPointId='
# url3 = '&app_id=4b54b268&app_key=cecb6d43290dda6a86edea437a15b536'
# url = paste(url1,line,url2,stopPointId,url3,sep = '')
# 
# #GET request and convert JSON to a dataframe
# data = GET(url)
# text_data = content(data,as = 'text')
# json_data = fromJSON(text_data)

#Define a function to convert seconds to min secs
minSec <-  function(x){
  
  ifelse(x<60,paste(x,'seconds',sep=' '),
         ifelse(x %% 60 == 0,paste(x/60, 'minutes',sep=' '),
                paste(floor(x/60),'mins',x %% 60, 'sec', sep = ' ')
                )
         )
}

#Define a function to return the last character of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Define a function to return the decision based on a letter
boundDirect <- function(x){
  ifelse(x == 'E','Eastbound',
         ifelse(x == 'W','Westbound',
                ifelse(x == 'N', 'Northbound',
                       'Southbound')))
}

#creating the line, station name lookup table

StationNodesDescription <- read.csv("StationNodesDescription.csv")

Line_Station = StationNodesDescription %>%
  select(Line,Station,NAPTAN) %>%
  filter(Line != '') %>%
  distinct() %>%
  arrange(Line)

#Define UI

ui = fluidPage(
  titlePanel(h1("London Underground Arrival Prediction", align = 'center'),br()),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('line', 
                  label = 'Select a line',
                  choices = c(
                    'Bakerloo',
                    'Central',
                    'Circle',
                    'District',
                    'Hammersmith & City',
                    'Jubilee',
                    'Metropolitan',
                    'Northern',
                    'Piccadilly',
                    'Victoria',
                    'Waterloo & City'
                  ),
                  selected = 'Piccadilly'),
      uiOutput('stop'),
      uiOutput('direction')
    ),
    mainPanel(
      tableOutput('tubeArrival')
    )
  )
)

#Define server logic
server <-  function(input,output){
  
  output$stop <-  renderUI({
    stationNames = filter(Line_Station, Line == input$line) %>%
      select(Station)
    selectInput('stop', label = 'Select a Station', stationNames, selected = 'South Kensington')
  })
  
  #the interactive direction select box
  output$direction = renderUI({
    
    #setting up the GET request url varibales
    url1 <-  'https://api.tfl.gov.uk/Line/'
    line <-  input$line
    stopPointId <-  as.character(
      unique(Line_Station$NAPTAN[Line_Station$Station == input$stop])
    )
    url2 <-  '/Arrivals?stopPointId='
    url3 <-  '&app_id=4b54b268&app_key=cecb6d43290dda6a86edea437a15b536'
    url <-  paste(url1,line,url2,stopPointId,url3,sep = '')
    
    #GET request and convert JSON to a dataframe
    data = GET(url)
    text_data = content(data,as = 'text')
    json_data = fromJSON(text_data)
    json_data$bound <-  substr(as.character(json_data$platformName),1,1)
    selectInput('direction', label = 'Select a Direction', boundDirect(unique(json_data$bound)))
  })
  
  output$tubeArrival <-  renderTable({

    #setting up the GET request url varibales
    url1 <-  'https://api.tfl.gov.uk/Line/'
    line <-  input$line
    stopPointId <-  as.character(
                                unique(Line_Station$NAPTAN[Line_Station$Station == input$stop])
                              )
    url2 <-  '/Arrivals?stopPointId='
    url3 <-  '&app_id=4b54b268&app_key=cecb6d43290dda6a86edea437a15b536'
    url <-  paste(url1,line,url2,stopPointId,url3,sep = '')

    #GET request and convert JSON to a dataframe
    data <-  GET(url)
    text_data <-  content(data,as = 'text')
    json_data <-  fromJSON(text_data)
    json_data$timeToArrive = minSec(json_data$timeToStation)
    json_data <- json_data[order(json_data$timeToStation),]
    json_data$bound <-  substr(as.character(json_data$platformName),1,1)
    json_data$platform <-  substrRight(as.character(json_data$platformName),1)
    cleaned_data <- subset(json_data, boundDirect(json_data$bound) == input$direction)
    # final_data <-  cleaned_data[,c('platform','towards','timeToArrive','currentLocation')]
    final_data <-  data.frame(cleaned_data$platform,cleaned_data$towards,cleaned_data$timeToArrive,cleaned_data$currentLocation,check.names = TRUE)
    names(final_data) <- c('Plat.','To','ETA','Current Loc.')
     # final_data <- rename(final_data,'A' = 'cleaned_data.platform',
     #                         'T' = 'cleaned_data.towards',
     #                         'E' = 'cleaned_data.timeToArrive',
     #                          'C' = 'cleaned_data.currentLocation')
    final_data <- final_data
  },
  striped = TRUE
  , hover = TRUE
  , align = 'c')
  
}

#Run the app
shinyApp(ui = ui, server = server)