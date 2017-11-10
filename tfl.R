#setting the shiny app environment
library(rsconnect)

rsconnect::setAccountInfo(name='zhuohao',
                          token='15789238E9BB10E18039583392933E45',
                          secret='Mi6QMCfVsQ7rxXhMnYj/7isdK9Kq1gxEoraXowgZ')

library(jsonlite)
library(httr)

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
minSec = function(x){
  if(x<60){
    min_Sec = paste(x,'seconds',sep=' ')
  }else if (x %% 60 == 0) {
    min_Sec = paste(x/60, 'minutes',sep=' ')
  }else
    min_Sec = paste(floor(x/60),'mins',x %% 60, 'sec', sep = ' ')
  return(min_Sec)
}

library(dplyr)

#creating the line, station name lookup table
Line_Station = StationNodesDescription %>%
  select(Line,Station,NAPTAN) %>%
  filter(Line != '') %>%
  distinct() %>%
  arrange(Line)

library(shiny)

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
      uiOutput('stop')
    ),
    mainPanel(
      tableOutput('tubeArrival')
    )
  )
)

#Define server logic
server = function(input,output){
  
  output$stop = renderUI({
    stationNames = filter(Line_Station, Line == input$line) %>%
      select(Station)
    selectInput('stop', label = 'Select a Station', stationNames, selected = 'South Kensington')
  })
  
  output$tubeArrival = renderTable({

    #setting up the GET request url varibales
    url1 = 'https://api.tfl.gov.uk/Line/'
    line = input$line
    stopPointId = as.character(
                                unique(Line_Station$NAPTAN[Line_Station$Station == input$stop])
                              )
    url2 = '/Arrivals?stopPointId='
    url3 = '&app_id=4b54b268&app_key=cecb6d43290dda6a86edea437a15b536'
    url = paste(url1,line,url2,stopPointId,url3,sep = '')

    #GET request and convert JSON to a dataframe
    data = GET(url)
    text_data = content(data,as = 'text')
    json_data = fromJSON(text_data)
    final_data = json_data[c('platformName','towards','currentLocation','timeToStation')]
  })
  
}

#Run the app
shinyApp(ui = ui, server = server)