library(shiny)
library(tidyverse)
#----- Loading the data -----
# countyDat <- read.csv(text=RCurl::getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
stateDat <- read.csv(text = RCurl::getURL("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))
fluidPage(
  titlePanel("COVID-Tracker"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stateData",
        label = "Select A State",
        choices = c(
          "All",
          as.character(unique(stateDat$state))
        )
      ),
      selectInput("date",
        label = "Select A Date",
        choices = c(as.character(unique(stateDat$date)))
      )
    ),
    mainPanel(
      plotOutput(outputId = "usaPlot")
    )
  )
)
