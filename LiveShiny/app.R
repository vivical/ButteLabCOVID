

######################################################################
######################################################################
#    PreProcessing
######################################################################
######################################################################

######################################################################
# Setup Notebook
######################################################################

# Load Libraries
library(shinydashboard)
library(usmap)
library(tidyverse)
library(tidycensus)
library(maps)
library(sf)
library(ggrepel)
library(stringr)
library(shinyjs)
library(fmsb) #for percentile
library(scales) #so we dont have scientific notation
devtools::install_github("UrbanInstitute/urbnmapr") # Uncomment
library(urbnmapr) #devtools::install_github("UrbanInstitute/urbnmapr")
# library(mapproj) albers projection? -- seems non-trivial

# Load Elliott Libraries
library(plotly)
library(ggplot2)

# Read in the NY Times Data
stateDat <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) # Uncomment
countyDat <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) # Uncomment

# stateDat <- read_csv(file = "./DataFiles/us-states.csv") # Comment
# countyDat <- read_csv(file = "./DataFiles/us-counties.csv") # Comment

# Remove the unknown counties -- we don't know where to assign these
countyDat <- countyDat %>%
  filter(county != "Unknown")

# Remove territories
territories = c("Guam", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands")
stateDat = stateDat[-which(stateDat$state %in% territories),]

#class(stateDat)
#View(stateDat)  # Puts data in R Studio view

######################################################################
# Clean the NY Times Data
######################################################################
# Fix NYC
countyDat[which(countyDat$county == "New York City" & countyDat$state == "New York"),"fips"] <- "NYC"
# Fix KC Mo -- combine all the counties that intersect KC with KC
kcCounties = c("Cass", "Clay", "Jackson", "Platte", "Kansas City")
kcCountyData = countyDat[which(countyDat$county %in% kcCounties & countyDat$state == "Missouri"),]

kcCountyData = kcCountyData %>%
  select(-c(county,state,fips))

kcCountyData <- kcCountyData %>%
  group_by(date) %>%
  summarise_all(sum)

kcCountyData = tibble(date = kcCountyData$date, county = "Kansas City", state = "Missouri", fips = "KC", cases = kcCountyData$cases, deaths = kcCountyData$deaths)
countyDat = countyDat[-which(countyDat$county %in% kcCounties & countyDat$state == "Missouri"),]
countyDat = rbind(countyDat,kcCountyData)

######################################################################
# Use US Census API to get Population Numbers
######################################################################
# # Get census API key from: https://api.census.gov/data/key_signup.html
# census_api_key("779c8b33af41fa4dbf19a22405c65c780fd379ac")
# 
# # Get population estimates per county from the US Census API
# popEst <- get_estimates(geography = "county", product = "population")
# popEst <- popEst %>%
#   filter(variable == "POP")
# 
# # Fix NYC
# nycCounties = c("New York County, New York", "Kings County, New York", "Queens County, New York", "Bronx County, New York", "Richmond County, New York")
# nycCounts = sum(popEst[which(popEst$NAME %in% nycCounties),"value"])
# nycCounts = tibble(NAME = "New York City, New York", GEOID = "NYC", variable = "POP", value = nycCounts)
# popEst = popEst[-which(popEst$NAME %in% nycCounties),]
# popEst = rbind(popEst,nycCounts)
# 
# # Fix KC Mo -- here, combine Cass, Clay, Jackson and Platte with KC
# kcCounties = c("Cass County, Missouri", "Clay County, Missouri", "Jackson County, Missouri", "Platte County, Missouri")
# kcCounts = sum(popEst[which(popEst$NAME %in% kcCounties),"value"])
# kcCounts = tibble(NAME = "Kansas City, Missouri", GEOID = "KC", variable = "POP", value = kcCounts)
# popEst = popEst[-which(popEst$NAME %in% kcCounties),]
# popEst = rbind(popEst,kcCounts)

popEst <- read_csv(file = "./DataFiles/census_county_populations.csv")
# popEst <- read_csv(file = "./LiveShiny/DataFiles/census_county_populations.csv")

statePopEst <- popEst %>% 
  separate(NAME, c("County", "State"), sep = ", ") %>%
  group_by(State) %>% 
  summarise(value = sum(value))

######################################################################
# Get the number of new cases and deaths per day
######################################################################
# New cases per day
countyDat <- countyDat %>% 
  group_by(county,state,fips) %>% 
  mutate(NewCases = cases - lag(cases, default = 0, order_by = date))

#New deaths per day
countyDat <- countyDat %>% 
  group_by(county,state,fips) %>% 
  mutate(NewDeaths = deaths - lag(deaths, default = 0, order_by = date))

#Now for the states
stateDat <- stateDat %>% 
  group_by(state) %>% 
  mutate(NewCases = cases - lag(cases, default = 0, order_by = date))
stateDat <- stateDat %>% 
  group_by(state) %>% 
  mutate(NewDeaths = deaths - lag(deaths, default = 0, order_by = date))

######################################################################
# Join the US Census data with the NY Times data
######################################################################
# Join Data
countyDat <- left_join(countyDat, popEst, by = c("fips" = "GEOID"))

countyDat <- countyDat %>%
  filter(!is.na(value)) # Here we are removing all the "Unknown" -- need to update this.... potentially use some of Paul's suggestions?

# Get cases and deaths per million population
countyDat <- countyDat %>%
  mutate(casesPerMillion = (cases/value)*1000000) %>%
  mutate(deathsPerMillion = (deaths/value)*1000000) %>%
  mutate(NewCasesPerMillion = (NewCases/value)*1000000) %>%
  mutate(NewDeathsPerMillion = (NewDeaths/value)*1000000)

# Now do for the states
stateDat <- left_join(stateDat, statePopEst, by = c("state" = "State"))
# Get cases and deaths per million population
stateDat <- stateDat %>%
  mutate(casesPerMillion = (cases/value)*1000000) %>%
  mutate(deathsPerMillion = (deaths/value)*1000000) %>%
  mutate(NewCasesPerMillion = (NewCases/value)*1000000) %>%
  mutate(NewDeathsPerMillion = (NewDeaths/value)*1000000)

######################################################################
# Set t = 0 to the first observed case in each county
######################################################################
# Set t=0 to the data of >= 10 cases
suppressWarnings( # this is noisy for N/As
  time_zero <- countyDat %>%
    group_by(state, county) %>%
    summarise(first_case = min(date[which(cases>=10)])) %>%
    ungroup
)


# Set a new column for the time elapsed between the date column and the t=0 date for each row
countyDat <- countyDat %>%
  left_join(time_zero, by = c("state", "county")) %>%
  mutate(time = as.numeric(date - first_case))

######################################################################
# Try to speed up plotly
######################################################################
#Make sure we include: plotly-redirect-cdn-1.39.2.js
# from: https://www.timlrx.com/2019/12/17/speeding-up-r-plotly-webapps-r-x-javascript-part-i/
# https://www.r-bloggers.com/speeding-up-r-plotly-web-apps-r-x-javascript-part-i/

# plotly_mod_dep = function(p){
#   deps <- p$dependencies
#   deps_urls <- purrr::map(
#     deps,
#     ~if(.x$name == "plotly-basic") {
#       .x$src = list(file=getwd())
#       .x$script = "plotly-redirect-cdn-1.39.2.js"
#       .x
#     } else {
#       .x
#     }
#   )
#   p$dependencies <- deps_urls
#   p
# }

######################################################################
######################################################################
#  UI 
######################################################################
######################################################################

#dim(stateDat)
#  Original State Data dimensions: 1437    5
# Formatted State Data dimensions: 1437    5
#  Original County Data dimensions: 17731     6
# Formatted County Data dimensions: 17458    18
addResourcePath("www", paste(getwd() , "/www", sep="") )

ui = fluidPage( style='margin-left:5px; margin-right:5px', title="COVID-19 County Tracker",
                
                tags$head(includeHTML("www/google_analytics.html")), # Add google analytics for tracking site
                tags$head(tags$link(rel="shortcut icon", href="www/virus2_icon.svg")),  # Icon to display in browser tab
                
                
                ######################################################################
                #  Title Section
                ######################################################################
                #div(class="row", div(class="jumbotron", style="background-color: white; padding: .2em", 
                #div( class="col-xs-2 col-md-1 col-lg-1",
                #      img(src='www/covid2.png', class="img-responsive")
                #),
                #div( #class="col-xs-10 col-md-11 col-lg-11",
                #      h1( class="text-center", "COVID-19 County", HTML("<i><font color='Crimson'>Tracker</font></i>") ) #,
                #HTML( "<h1 class='text-center'><i><font color='Crimson'>Tracker</font></i></h1>")
                #),
                #div( class="col-xs-12 col-md-6 col-lg-6",
                #      HTML("<hr style='padding:0px; margin:0px'>")
                #)
                #)),
                div(class="jumbotron", style="background-color: white; padding-left:.4em; padding-right:.4em; padding-bottom:.1em; padding-top: .6em",
                    
                    div( #class="col-xs-10 col-md-11 col-lg-11",
                      h1( class="text-center", style="padding-top:0px;margin-top:0px", "COVID-19 County Tracker") #, HTML("<i><font color='Crimson'>Tracker</font></i>") ) #,
                    ),
                    div( class="col-xs-12 col-md-offset-2 col-md-8 col-lg-offset-3 col-lg-6",
                         HTML("<hr style='padding:0px; margin:0px'>")
                    ),  
                    
                    h3(class="text-center", "COVID-19 cases across all 3,142 counties in the United States" ),
                    p(class="text-center", style="padding-bottom:0px; margin-bottom:0px", 
                      "COVID-19 is rapidly spreading across the United States, but at different rates and intensities.  Medical professionals, leaders, and policy makers should have the most up-to-date information about cases and growth trends in their local region.  The ",
                      a(href="http://buttelab.ucsf.edu/", "Butte Lab",inline = T, target = "_blank"), " at UCSF has partnered with ", 
                      a(href="https://twitter.com/pbleic?s=20", "Paul Bleicher",inline = T, target = "_blank"),
                      "MD, PhD, former CEO of OptumLabs, to calculate and visualize COVID-19 statistics for every county in the United States. Use the tools below to view the most recent data for any region. For questons and feedback, please ",
                      a(href="mailto:covid.tracker@bakar.institute", "contact us.",inline = T, target = "_blank") )
                ),
                
                ######################################################################
                #  Sidebar
                ######################################################################
                div( class="container", #style="background-color:#F2F3F4;",
                     div(class="col-xs-12", align="center",  
                         tags$span( selectizeInput("cState", "Select a state:",
                                                   selected = "California", # default value
                                                   choices = c(sort(as.character(unique(stateDat$state))))),
                                    HTML('<button type="button" class="btn btn-default btn-xs" style="width: 200px ; height: 50px; font-size: 125%" data-toggle="collapse" data-target="#demo">Customization Options</button>')
                         )          
                         #HTML('<span><i class="fas fa-caret-down"></i></span>'),
                         #tags$button( icon=icon("caret-down"), label="", class="btn btn-default text-center",  data-toggle="collapse", data-target="#demo" )
                     ),
                     div( id="demo", class="collapse",
                          fluidRow(
                            div( class="col-sm-3 col-xs-6", radioButtons("uPlot", "Plot by:",
                                                                         selected = "cases", # default value
                                                                         choices = c("Total cases" = "cases",
                                                                                     "Total deaths" = "deaths",
                                                                                     "New cases" = "nCases",
                                                                                     "New deaths" = "nDeaths"))),
                            div( class="col-sm-3 col-xs-6", radioButtons("uTime", "Time Scale",
                                                                         selected = "absoluteT", # default value
                                                                         choices = c("Aligned (since first 10 cases)" = "realtiveT",
                                                                                     "Actual dates" = "absoluteT"))),
                            div( class="col-sm-3 col-xs-6", radioButtons("uScale", "Scale by Population Density:",
                                                                         selected = "false", # default vcalue
                                                                         choices = c("True" = "true",
                                                                                     "False" = "false"))),
                            div( class="col-sm-3 col-xs-6", radioButtons("pScale", "Y-Axis:",
                                                                         selected = "linear", # default value
                                                                         choices = c("Linear" = "linear",
                                                                                     "Log" = "log")))
                          ),
                          fluidRow(
                            column( 6,  
                                    numericInput("mCases", "Include counties with more cases than:", 
                                                 value = 25,# default value
                                                 min = 0, 
                                                 max = max(countyDat$cases))#,
                                    # p("View/hide a county by clicking it's name. Double-click to select only one county.", style="padding-top:0; margin-top:0"),
                            ),
                            column(6,
                                   dateRangeInput("daterange1", "Date Range (some data streams begin as early as 1/21/2020):", # (includes data from 1/1/20-Present)
                                                  start  = as.Date("2020-03-01"), # default value
                                                  end    = max(countyDat$date),
                                                  min    = min(countyDat$date),
                                                  max    = max(countyDat$date),
                                                  format = "mm/dd/yy",
                                                  separator = " - ")
                            )
                          ),
                          tags$hr()
                     ) # end "collapse" section 
                ), 
                
                ######################################################################
                #  Main Plot
                ######################################################################
                div( class="container-fluid",
                     h1(class="text-center", textOutput("state_title",inline = T) ),
                     # p(class="text-right", "View/hide a county by clicking it's name. Double-click to select only one county." , inline = T),
                     # plotOutput("stateLinePlot"),
                     plotlyOutput("stateLinePlot"),
                     div( class="col-xs-4", align="right", 
                          h5("Select Counties:", style="padding-top: 1.1em")
                     ),
                     div( class="col-xs-5",# align="center", 
                          #("Select County: " ,
                          uiOutput("secondSelection")
                          #selectInput( "cCounty", " ",
                          #             selected = "All", # default value
                          #             choices = c("1m","2,","3","All") )
                     )
                     #plotlyOutput("stateLinePlot")
                ), # ends "mainPanel" from "Main Plot" section
                
                # Scratch
                #county_list = (countyDat %>% filter(state==input$cState) %>% distinct(county) )$county
                #county_list$county
                
                
                ######################################################################
                #  US Map and State Map
                ######################################################################
                fluidRow(
                  div( class="col-xs-12 col-md-6", plotlyOutput("usaPlot") ),
                  # column(6, plotOutput("usaPlot") ),
                  div( class="col-xs-12 col-md-6", plotlyOutput("stateMapPlot") )#,
                  # column(6, plotOutput("stateMapPlot") )
                  # Removed this to only have one way to set dates
                  # column( 12, align="center",
                  #         sliderInput("daterange2", "Select Date",
                  #                     min = min(countyDat$date), 
                  #                     max = max(countyDat$date), 
                  #                     value = max(countyDat$date), # default value is the current date
                  #                     timeFormat="%Y-%m-%d") 
                  # )
                ) ,
                
                ######################################################################
                #  Footer Info
                ######################################################################
                div( #class="jumbotron", style="background-color: white; padding: .4em",
                  h3( "Data Sources" ), # class="text-center",
                  p(class="text-left",  "State and county infection data: ",
                    a(href="https://github.com/nytimes/covid-19-data", "New York Times COVID-19",inline = T, target = "_blank"), "github."),
                  p(class="text-left",  "US Census data: ",
                    a(href="https://www.census.gov/developers/", "US Census API",inline = T, target = "_blank"),
                    "and the",
                    a(href="https://walkerke.github.io/tidycensus/", "tidycensus",inline = T, target = "_blank"),
                    "R package."),
                  
                  fluidRow( p( class="col-xs-12 col-sm-7 col-md-5", style="padding-left:.8em;", strong("Team: "), # style="padding-left:0px",  margin-left:0px
                               a(href="mailto:douglas.arneson@ucsf.edu","Douglas Arneson, ",inline = T, target = "_blank"), 
                               a(href="https://twitter.com/atulbutte?s=20","Atul Butte, ",inline = T, target = "_blank"),
                               a(href="https://twitter.com/pbleic?s=20","Paul Bleicher, ",inline = T, target = "_blank"),
                               a(href="mailto:matthew.elliott@ucsf.edu","Matthew Elliott, ",inline = T, target = "_blank"),
                               a(href="mailto:arman.mosenia@ucsf.edu","Arman Mosenia, ",inline = T, target = "_blank"),
                               a(href="mailto:boris.oskotsky@ucsf.edu","Boris Oskotsky, ",inline = T, target = "_blank"),
                               a(href="mailto:vivek.rudrapatna@ucsf.edu","Vivek Rudrapatna, ",inline = T, target = "_blank"),
                               a(href="mailto:rohit.vashisht@ucsf.edu","Rohit Vashisht, ",inline = T, target = "_blank"),
                               a(href="mailto:travis.zack@ucsf.edu","Travis Zack",inline = T, target = "_blank")
                  )),
                  
                  div( class="col-xs-12 col-sm-6 col-md-4 col-lg-3",
                       img(src='www/bakar_logo.png', class="img-responsive")
                  )
                ),
                
                div(class="jumbotron", style="background-color: white", HTML("<h1>&nbsp</h1>") ),
                
                ######################################################################
                #  Leading Message
                ######################################################################
                # Loading message test
                tags$style( type="text/css", "
                             #loadmessage { position: fixed; top: 0px; left: 0px; width: 100%; padding: 5px 0px 5px 0px;
                               text-align: center; font-weight: bold; font-size: 100%; color: #000000;
                               background-color: #FE672E; z-index: 105;  }
                  "),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("Loading...",id="loadmessage")
                )
)



######################################################################
######################################################################
#    Server 
######################################################################
######################################################################

server = function(input, output, session) {
  
  #######################################################################
  #  Use plotly click events on the US Map to change the selected state
  #######################################################################
  # Added -- Doug
  observeEvent(event_data("plotly_click", source = "usaPlot"), {
    s <- event_data("plotly_click", source = "usaPlot")
    if (length(s) == 0) {
    } else {
      as.list(s)
      updateSelectizeInput(session, "cState",selected = str_to_title(s$key[[1]]))
    }
  })
  
  ######################################################################
  #  Get parameters from the URL
  ######################################################################
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['st']])) updateSelectizeInput(session, "cState",selected = query[['st']])
    if(!is.null(query[['pb']])) updateRadioButtons(session, "uPlot",selected = query[['pb']])
    if(!is.null(query[['ts']])) updateRadioButtons(session, "uTime",selected = query[['ts']])
    if(!is.null(query[['sc']])) updateRadioButtons(session, "uScale",selected = query[['sc']])
    if(!is.null(query[['ax']])) updateRadioButtons(session, "pScale",selected = query[['ax']])
    if(!is.null(query[['min']])) updateNumericInput(session, "mCases",value = query[['min']])
    if(!is.null(query[['dr1']])) updateDateRangeInput(session, "daterange1",start = query[['dr1']])
    if(!is.null(query[['dr2']])) updateDateRangeInput(session, "daterange1",end = query[['dr2']])
    # if(!is.null(query[['dr3']])) updateSliderInput(session, "daterange2",value = as.Date(query[['dr3']]))
  })
  
  ######################################################################
  #  Add parameters to the URL
  ######################################################################
  # observeEvent(c(input$cState,input$uPlot,input$uTime,input$uScale,input$pScale,input$mCases,input$daterange1,input$daterange2), {
  #   updateQueryString(paste0("?st=",input$cState,"&pb=",input$uPlot,"&ts=",input$uTime,"&sc=",input$uScale,"&ax=",input$pScale,"&min=",input$mCases,"&dr1=",input$daterange1[1],
  #                            "&dr2=",input$daterange1[2],"&dr3=",input$daterange2), mode = "replace")
  # })
  observeEvent(c(input$cState,input$uPlot,input$uTime,input$uScale,input$pScale,input$mCases,input$daterange1), {
    updateQueryString(paste0("?st=",input$cState,"&pb=",input$uPlot,"&ts=",input$uTime,"&sc=",input$uScale,"&ax=",input$pScale,"&min=",input$mCases,"&dr1=",input$daterange1[1],
                             "&dr2=",input$daterange1[2]), mode = "replace")
  })
  
  ######################################################################
  #  Add Counties to County Selector Input
  ######################################################################
  output$secondSelection <- renderUI({
    # Added Doug -- more flexible input (autofill and select multiple)
    selectizeInput( "cCounty", " ",
                    # selectInput( "cCounty", " ",
                    selected = "All",
                    choices = c("All", sort((countyDat %>% filter(state==input$cState) %>% distinct(county) )$county )),
                    multiple = TRUE
    )
  })
  
  ######################################################################
  #  Main Plot
  ######################################################################
  output$stateLinePlot <- renderPlotly({ #renderPlot({
    
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    
    # Create Name of state over graph
    output$state_title = renderText({  input$cState  })
    
    # Pull in the county data
    dat <- countyDat
    
    # This filters by the minimum number of cases
    selected_counties <- countyDat %>%
      group_by(state, county) %>%
      summarise(max_cases_per_county = max(cases)) %>%
      mutate(has_enough_cases = (max_cases_per_county > input$mCases)) %>%
      filter(has_enough_cases) %>%
      ungroup
    
    dat <- dat %>%
      left_join(selected_counties, by = c("state", "county")) %>%
      ungroup
    
    dat <- dat %>%
      # Filter by the counties by the selected state, the date range, and if we have enough cases
      filter(state == input$cState & 
               date >= input$daterange1[1] &
               date <= input$daterange1[2] &
               has_enough_cases)
    
    # Added by Elliott
    # Use cCounty input to select a single county when "All" is not selected
    if(length(input$cCounty) != 0){ # Added by Doug -- initially was throwing an error when the app loaded (b/c there is no value)
      if(!"All" %in% input$cCounty){
        dat <- dat %>% filter(county %in% input$cCounty )
      }
    }
    
    # Add a label for the max time for each county
    max_date_label <- dat %>%
      group_by(state, county) %>%
      summarise(max_time = max(time))
    
    dat <- dat %>%
      left_join(max_date_label, by = c("state", "county"))
    
    # Get the units we are actually plotting
    if(input$uPlot == "cases" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = casesPerMillion)
      annoYlab = "Cumulative Cases Per Million"
      annoTitle = paste0("Cumulative Cases Per Million in Each ",input$cState," County")
    }
    if(input$uPlot == "cases" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = cases)
      annoYlab = "Cumulative Cases"
      annoTitle = paste0("Cumulative Cases in Each ",input$cState," County")
    }
    if(input$uPlot == "nCases" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = NewCasesPerMillion)
      annoYlab = "New Cases Per Million"
      annoTitle = paste0("New Cases Per Million in Each ",input$cState," County")
    }
    if(input$uPlot == "nCases" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = NewCases)
      annoYlab = "New Cases"
      annoTitle = paste0("New Cases in Each ",input$cState," County")
    }
    if(input$uPlot == "deaths" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = deathsPerMillion)
      annoYlab = "Cumulative Deaths Per Million"
      annoTitle = paste0("Cumulative Deaths Per Million in Each ",input$cState," County")
    }
    if(input$uPlot == "deaths" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = deaths)
      annoYlab = "Cumulative Deaths"
      annoTitle = paste0("Cumulative Deaths in Each ",input$cState," County")
    }
    if(input$uPlot == "nDeaths" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = NewDeathsPerMillion)
      annoYlab = "New Deaths Per Million"
      annoTitle = paste0("New Deaths Per Million in Each ",input$cState," County")
    }
    if(input$uPlot == "nDeaths" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = NewDeaths)
      annoYlab = "New Deaths"
      annoTitle = paste0("New Deaths in Each ",input$cState," County")
    }
    
    # Check if we are using absolute or relative (to the first case) time
    if(input$uTime == "realtiveT"){
      dat <- dat %>%
        mutate(time = time) %>%
        # Only keep points with positive time (need >= 10 cases)
        filter(time >= 0) %>%
        # Label only the latest point
        mutate(label = if_else(time == max_time, as.character(county), NA_character_))
      xAxis <- scale_x_continuous(limits = c(min(dat$time),max(dat$time)+3))
    }
    if(input$uTime == "absoluteT"){
      dat <- dat %>%
        mutate(time = date) %>%
        # Label only the latest point
        mutate(label = if_else(date == max(date), as.character(county), NA_character_))
      xAxis <- scale_x_date(limits = c(min(dat$time),max(dat$time)+3), date_labels = "%m/%d")
    }
    
    # Set up the county labels
    labelDat = dat[-which(is.na(dat$label)),]
    
    # Order the counties in the lagend by max value
    maxValue <- dat %>%
      group_by(state, county) %>%
      summarise(max_value = max(value)) %>%
      ungroup() %>%
      arrange(desc(max_value))
    
    dat <- dat %>%
      mutate(county = factor(county, levels = unique(maxValue$county)))
    
    # Set up the y-axis scale
    if(input$pScale == "linear"){
      # plotScale = scale_y_continuous(trans='identity',labels = function(x) format(x[!is.na(x)], scientific = F))
      scaleAnno = "Linear Plot"
    }
    # if(input$pScale == "log"){
    #   # plotScale = scale_y_continuous(trans='log10',labels = function(x) format(x[!is.na(x)], scientific = F))
    # 
    # }
    
    # If log scale add doubling lines
    if(input$pScale == "log"){
      if(input$uTime == "absoluteT"){
        startVal = 1
        addVal = 0
      }
      if(input$uTime == "realtiveT" & input$uPlot == "cases"){
        startVal = 8
        addVal = 3
      }
      if(input$uTime == "realtiveT" & input$uPlot != "cases"){
        startVal = 1
        addVal = 0
      }
      # Build doubling time lines
      allDates = sort(unique(dat$time))
      everyTwoDays = c(startVal)
      everyTwoDaysNames = c(allDates[1])
      everyFourDays = c(startVal)
      everyFourDaysNames = c(allDates[1])
      for(i in 2:as.numeric(max(dat$time)-min(dat$time))){
        if((i-1)%%2 == 0){
          everyTwoDays = c(everyTwoDays,2^((i-1)/2+addVal))
          # everyTwoDays = c(everyTwoDays,everyTwoDays[i-1]*2)
          everyTwoDaysNames = c(everyTwoDaysNames,allDates[i])
        }
        if((i-1)%%4 == 0){
          everyFourDays = c(everyFourDays,2^((i-1)/4+addVal))
          # everyFourDays = c(everyFourDays,everyFourDays[i-1]*2)
          everyFourDaysNames = c(everyFourDaysNames,allDates[i])
        }
      }
      
      everyTwoDays = tibble(time = everyTwoDaysNames, value = everyTwoDays, county = "Doubling every 2 days")
      everyFourDays = tibble(time = everyFourDaysNames, value = everyFourDays, county = "Doubling every 4 days")
      
      # To make sure we don't have scientific notation
      labelFormat <- label_number(big.mark = ",", accuracy = 1)
      
      if(input$uTime == "absoluteT"){
        plotScale = scale_y_continuous(trans='log10', limits = c(1, round(max(c(max(dat$value),max(everyTwoDays$value),max(everyFourDays$value))))),
                                       labels = labelFormat)
        #,labels=function(n){format(n, scientific = FALSE)})
      }
      if(input$uTime == "realtiveT" & input$uPlot == "cases" & input$uScale == "false"){
        plotScale = scale_y_continuous(trans='log10', limits = c(10, round(max(c(max(dat$value),max(everyTwoDays$value),max(everyFourDays$value))))),
                                       labels = labelFormat)
        #,labels=function(n){format(n, scientific = FALSE)})
      }
      if(input$uTime == "realtiveT" & (input$uPlot != "cases" | input$uScale == "true")){
        plotScale = scale_y_continuous(trans='log10', limits = c(1, round(max(c(max(dat$value),max(everyTwoDays$value),max(everyFourDays$value))))),
                                       labels = labelFormat)
        #,labels=function(n){format(n, scientific = FALSE)})
      }
      # placeholder
      scaleAnno = "Log Plot"
      
      # filter all zeros for the log plot
      dat <- dat %>%
        filter(value > 0)
      
      #Silence the aesthetics we use for plotly but not directly in ggplot
      suppressWarnings(
        p1 <- dat %>%
          ggplot(aes(x = time, y = value, group = county, color = county,
                     text = paste0("Value: ", round(value), "</br></br>County: ", county, "</br>Time: "))) +
          geom_line() +
          geom_line(data = everyTwoDays, color = "black", linetype = "longdash" ) +
          geom_line(data = everyFourDays, color = "black", linetype = "longdash" ) +
          # xlim(min(dat$time),max(dat$time)+3) +
          geom_text(data = labelDat, aes(label = label), nudge_x = 0.15) +
          annotate(geom="text", x = max(everyTwoDays$time) + 0.15, y=max(everyTwoDays$value), label="Doubling every 2 days", color="black") +
          annotate(geom="text", x = max(everyFourDays$time) + 0.15, y=max(everyFourDays$value), label="Doubling every 4 days", color="black") +
          # geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) + # this does not work in plotly
          ggtitle(label = paste0(annoTitle," - ",scaleAnno), subtitle = paste0("minimum each county is ",input$mCases," cases")) +
          # xlab(paste0("Dates from: ",format(input$daterange1[1],"%m/%d")," - ",format(input$daterange1[2], "%m/%d"))) +
          ylab(annoYlab) +
          xAxis + # x-axis labels (time format)
          plotScale + #y-axis scale
          theme_bw(base_size = 12) +
          theme(legend.title=element_blank())
        #theme(legend.position = "none")
      )
      
      ggplotly(p1, tooltip = "text") %>%
        style(textposition = "right") %>%
        layout(title = list(text = paste0(annoTitle," - ",scaleAnno,
                                          '<br>',
                                          '<sup>',
                                          'View/hide a county by clicking it\'s name in the legend. Double-click to select only one county.',
                                          '</sup>'))) #%>%
        # partial_bundle(local = FALSE) %>%
        # plotly_mod_dep() #%>%
        # toWebGL()
      
    } else{
      #Silence the aesthetics we use for plotly but not directly in ggplot
      suppressWarnings(
        # Otherwise, do not add the lines
        p1 <- dat %>%
          ggplot(aes(x = time, y = value, group = county, color = county, text = paste0("Value: ", round(value), "</br></br>County: ", county, "</br>Time: ", time))) + 
          geom_line() +
          # xlim(min(dat$time),max(dat$time)+3) +
          geom_text(data = labelDat, aes(label = label), nudge_x = 0.15) +
          # geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) + # this does not work in plotly
          ggtitle(label = paste0(annoTitle," - ",scaleAnno), subtitle = paste0("minimum each county is ",input$mCases," cases")) +
          xlab(paste0("Dates from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"))) +
          ylab(annoYlab) +
          xAxis + # x-axis labels (time format)
          # plotScale + #y-axis scale
          theme_bw(base_size = 12) +
          theme(legend.title=element_blank())
        #theme(legend.position = "none")
      )
      
      ggplotly(p1, tooltip = "text") %>%
        style(textposition = "right") %>%
        layout(title = list(text = paste0(annoTitle," - ",scaleAnno,
                                          '<br>',
                                          '<sup>',
                                          'View/hide a county by clicking it\'s name in the legend. Double-click to select only one county.',
                                          '</sup>'))) #%>%
        # partial_bundle(local = FALSE) %>%
        # plotly_mod_dep() #%>%
        # toWebGL()
      
      # toWebGL(p1)
    
    }
    
  })
  
  ######################################################################
  #  US Map
  ######################################################################
  
  output$usaPlot <- renderPlotly({
    
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    
    # Get the appropriate date range
    dat <- stateDat
    dat1 <- subset(dat, as.character(date) == as.character(as.Date(input$daterange1[1])) )
    dat <- subset(dat, as.character(date) == as.character(as.Date(input$daterange1[2])) )
    # Find the states that were missing before
    toAdd = dat[which(!dat$state %in% dat1$state),]
    if(nrow(toAdd)>0){
      toAdd[,c("cases","deaths")] = 0
      dat1 = rbind(dat1,toAdd)
    }
    dat <- dat %>% 
      arrange(state)
    dat1 <- dat1 %>% 
      arrange(state)
    
    # Get the difference in the date ranges
    dat$cases = dat$cases - dat1$cases
    dat$deaths = dat$deaths - dat1$deaths
    
    
    # dat <- dat %>%
    #   mutate(value = cases)
    
    # # Get the units we are actually plotting
    # if(input$uPlot == "cases" | input$uPlot == "nCases"){
    #   dat <- dat %>%
    #     mutate(value = cases)
    #   annoTitle = paste0("Cumulative Cases in Each State from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"))
    # }
    # if(input$uPlot == "deaths" | input$uPlot == "nDeaths"){
    #   dat <- dat %>%
    #     mutate(value = deaths)
    #   annoTitle = paste0("Cumulative Deaths in Each State from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"))
    # }
    
    # Get the units we are actually plotting
    if(input$uPlot == "cases" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = casesPerMillion)
      annoTitle = paste0("Cumulative Cases Per Million in Each State from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"))
    }
    if(input$uPlot == "cases" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = cases)
      annoTitle = paste0("Cumulative Cases in Each State from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"))
    }
    if(input$uPlot == "nCases" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = NewCasesPerMillion)
      annoTitle = paste0("New Cases Per Million in Each State on: ",format(input$daterange1[2], "%m/%d"))
    }
    if(input$uPlot == "nCases" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = NewCases)
      annoTitle = paste0("New Cases in Each State on: ",format(input$daterange1[2], "%m/%d"))
    }
    if(input$uPlot == "deaths" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = deathsPerMillion)
      annoTitle = paste0("Cumulative Deaths Per Million in Each State from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"))
    }
    if(input$uPlot == "deaths" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = deaths)
      annoTitle = paste0("Cumulative Deaths in Each State from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"))
    }
    if(input$uPlot == "nDeaths" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = NewDeathsPerMillion)
      annoTitle = paste0("New Deaths Per Million in Each State on: ",format(input$daterange1[2], "%m/%d"))
    }
    if(input$uPlot == "nDeaths" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = NewDeaths)
      annoTitle = paste0("New Deaths in Each State on: ",format(input$daterange1[2], "%m/%d"))
    }
    
    # convert data to percentiles
    perc.rank <- function(x) trunc(rank(x))/length(x)
    dat$value = perc.rank(dat$value)
    
    # dat <- dat %>%
    #   mutate(value = percentile(value))
    
    states_sf <- get_urbn_map(map = "states", sf = TRUE)
    
    dat <- states_sf %>% 
      left_join(dat, by = c("state_name" = "state"))
    
    # Hover does not work if these are exactly the same color, shift them slightly
    duplicatedVals = unique(dat$value[!is.na(dat$value)][duplicated(dat$value[!is.na(dat$value)])])
    if(length(duplicatedVals)>0){
      for(duplicatedVal in duplicatedVals){
        duplicatedIdents = which(dat$value == duplicatedVal)
        dat$value[duplicatedIdents] = dat$value[duplicatedIdents] + 0.001*duplicatedIdents
      }
    }
    
    stateSelect <- dat[which(dat$state_name == input$cState),]
    
    # By date
    #Silence the aesthetics we use for plotly but not directly in ggplot
    suppressWarnings(
      p2 <- ggplot() +
        geom_sf(data = dat, mapping = aes(fill = value, state = state_name, key = state_name, cases = cases, deaths = deaths, casesPerMillion = casesPerMillion, deathsPerMillion = deathsPerMillion, NewCases = NewCases, 
                                          NewDeaths = NewDeaths, NewCasesPerMillion = NewCasesPerMillion, NewDeathsPerMillion = NewDeathsPerMillion,
                                          # text = paste0("State: ", str_to_title(state_name), "</br></br>Cases: ", format(cases,big.mark=",",scientific=FALSE), "</br>Deaths: ", format(deaths,big.mark=",",scientific=FALSE)
                                          text = paste0("State: ", str_to_title(state_name),
                                                        "</br></br>Cases (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(cases,big.mark=",",scientific=FALSE),
                                                        "</br>Deaths (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(deaths,big.mark=",",scientific=FALSE),
                                                        "</br>Cases Per Million (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(round(casesPerMillion),big.mark=",",scientific=FALSE),
                                                        "</br>Deaths Per Million (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(round(deathsPerMillion),big.mark=",",scientific=FALSE),
                                                        "</br>New Cases (",format(input$daterange1[2], "%m/%d"),"): ", format(NewCases,big.mark=",",scientific=FALSE),
                                                        "</br>New Deaths (",format(input$daterange1[2], "%m/%d"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE),
                                                        "</br>New Cases Per Million (",format(input$daterange1[2], "%m/%d"),"): ", format(NewCasesPerMillion,big.mark=",",scientific=FALSE),
                                                        "</br>New Deaths Per Million (",format(input$daterange1[2], "%m/%d"),"): ", format(NewDeathsPerMillion,big.mark=",",scientific=FALSE)
                                          )),
                color = "gray25", size = 0.5) +
        geom_sf(data = stateSelect, mapping = aes(fill = value, state = state_name, key = state_name, cases = cases, deaths = deaths, casesPerMillion = casesPerMillion, deathsPerMillion = deathsPerMillion, NewCases = NewCases, 
                                                  NewDeaths = NewDeaths, NewCasesPerMillion = NewCasesPerMillion, NewDeathsPerMillion = NewDeathsPerMillion,
                                                  # text = paste0("State: ", str_to_title(state_name), "</br></br>Cases: ", format(cases,big.mark=",",scientific=FALSE), "</br>Deaths: ", format(deaths,big.mark=",",scientific=FALSE)
                                                  text = paste0("State: ", str_to_title(state_name), 
                                                                "</br></br>Cases (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(cases,big.mark=",",scientific=FALSE),
                                                                "</br>Deaths (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(deaths,big.mark=",",scientific=FALSE),
                                                                "</br>Cases Per Million (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(round(casesPerMillion),big.mark=",",scientific=FALSE),
                                                                "</br>Deaths Per Million (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(round(deathsPerMillion),big.mark=",",scientific=FALSE),
                                                                "</br>New Cases (",format(input$daterange1[2], "%m/%d"),"): ", format(NewCases,big.mark=",",scientific=FALSE),
                                                                "</br>New Deaths (",format(input$daterange1[2], "%m/%d"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE),
                                                                "</br>New Cases Per Million (",format(input$daterange1[2], "%m/%d"),"): ", format(NewCasesPerMillion,big.mark=",",scientific=FALSE),
                                                                "</br>New Deaths Per Million (",format(input$daterange1[2], "%m/%d"),"): ", format(NewDeathsPerMillion,big.mark=",",scientific=FALSE)
                                                  )),
                color = "cyan", size = 1) +
        theme_bw() +
        # scale_fill_gradient2(midpoint = median(dat$value), low = "#0571b0", mid = "#f7f7f7", high = "#ca0020", na.value="gray75",
        #                      labels=function(n){paste0((n*100),"th")}) +
        scale_fill_gradient2(midpoint = median(dat$value), low = "white", mid = "#ffbe87", high = "#e6550d", na.value="gray75",
                             labels=function(n){paste0((n*100),"th")}) +
        labs( fill = "Percentile Rank" ) + #edit the legend title
        ggtitle(annoTitle) +
        theme(
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title = element_blank()
        )
    )
    ggplotly(p2, tooltip = "text", source = "usaPlot") %>% style(hoveron = "key") %>% event_register("plotly_click") #%>%
      # partial_bundle(local = FALSE) %>%
      # plotly_mod_dep() #%>%
      # toWebGL()
    # toWebGL(p2)
    
  })
  
  ######################################################################
  #  State Map
  ######################################################################
  
  output$stateMapPlot <- renderPlotly({
    # output$stateMapPlot <- renderPlot({
    
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    
    newStart = as.Date(input$daterange1[1])
    newEnd = as.Date(input$daterange1[2])
    stateUse = input$cState  #input$mState
    dat <- countyDat
    
    # Get the units we are actually plotting
    if(input$uPlot == "cases" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = casesPerMillion)
      annoTitle = paste0("Cumulative Cases Per Million in Each ",input$cState," County from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    if(input$uPlot == "cases" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = cases)
      annoTitle = paste0("Cumulative Cases in Each ",input$cState," County from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    if(input$uPlot == "nCases" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = NewCasesPerMillion)
      annoTitle = paste0("New Cases Per Million in Each ",input$cState," County on: ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    if(input$uPlot == "nCases" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = NewCases)
      annoTitle = paste0("New Cases in Each ",input$cState," County on: ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    if(input$uPlot == "deaths" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = deathsPerMillion)
      annoTitle = paste0("Cumulative Deaths Per Million in Each ",input$cState," County from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    if(input$uPlot == "deaths" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = deaths)
      annoTitle = paste0("Cumulative Deaths in Each ",input$cState," County from: ",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    if(input$uPlot == "nDeaths" & input$uScale == "true"){
      dat <- dat %>%
        mutate(value = NewDeathsPerMillion)
      annoTitle = paste0("New Deaths Per Million in Each ",input$cState," County on: ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    if(input$uPlot == "nDeaths" & input$uScale == "false"){
      dat <- dat %>%
        mutate(value = NewDeaths)
      annoTitle = paste0("New Deaths in Each ",input$cState," County on: ",format(input$daterange1[2], "%m/%d"), "\n (percentile ranks are relative to counties within the state)")
    }
    
    counties_sf <- get_urbn_map("counties", sf = TRUE)
    counties_sf$county_name = gsub(pattern = " County", replacement = "", x = counties_sf$county_name)
    # Fix for NYC
    nycCounties = c("New York","Kings","Queens","Bronx","Richmond")
    
    # Throws a warning when we modify: st_crs<- : replacing crs does not reproject data; use st_transform for that
    # This is ok
    suppressWarnings(counties_sf[which(counties_sf$state_name == "New York" & counties_sf$county_name %in% nycCounties),]$county_name <- "New York City")
    suppressWarnings(counties_sf[which(counties_sf$state_name == "New York" & counties_sf$county_name == "New York City"),]$county_fips <- "NYC")
    
    # Fix for KC
    kcCounties = c("Cass", "Clay", "Jackson", "Platte", "Kansas City")
    suppressWarnings(counties_sf[which(counties_sf$state_name == "Missouri" & counties_sf$county_name %in% kcCounties),]$county_name <- "Kansas City")
    suppressWarnings(counties_sf[which(counties_sf$state_name == "Missouri" & counties_sf$county_name == "Kansas City"),]$county_fips <- "KC")
    counties_sf <- subset(counties_sf, state_name == stateUse)
    
    # Join the state spatial data with the covid-19 data
    # Get the appropriate date range
    dat1 = dat[which(dat$state == stateUse & dat$date == newStart),]
    dat = dat[which(dat$state == stateUse & dat$date == newEnd),]
    # Find the counties that were missing before
    toAdd = dat[which(!dat$county %in% dat1$county),]
    if(nrow(toAdd)>0){
      toAdd[,c("cases","deaths","casesPerMillion","deathsPerMillion")] = 0
      dat1 = rbind(dat1,toAdd)
    }
    dat <- dat %>% 
      arrange(county)
    dat1 <- dat1 %>% 
      arrange(county)
    
    # Get the difference in the date ranges
    dat$cases = dat$cases - dat1$cases
    dat$deaths = dat$deaths - dat1$deaths
    dat$casesPerMillion = dat$casesPerMillion - dat1$casesPerMillion
    dat$deathsPerMillion = dat$deathsPerMillion - dat1$deathsPerMillion
    
    stateSubset = dat
    
    # stateSubset = dat[which(dat$state == stateUse & dat$date == newStart),]
    
    
    # Percentile within the state
    perc.rank <- function(x) trunc(rank(x))/length(x)
    stateSubset$value = perc.rank(stateSubset$value)
    
    stateSubset <- left_join(counties_sf, stateSubset, by = c("county_fips" = "fips"))
    
    # Rename columns for plotting
    stateSubset <- stateSubset %>%
      mutate(State = state_name) %>%
      mutate(County = county_name) %>%
      # Capitalize the county and sate names
      mutate(County = str_to_title(County)) %>%
      mutate(State = str_to_title(State))
    
    # Hover does not work if these are exactly the same color, shift them slightly
    duplicatedVals = unique(stateSubset$value[!is.na(stateSubset$value)][duplicated(stateSubset$value[!is.na(stateSubset$value)])])
    if(length(duplicatedVals)>0){
      for(duplicatedVal in duplicatedVals){
        duplicatedIdents = which(stateSubset$value == duplicatedVal)
        stateSubset$value[duplicatedIdents] = stateSubset$value[duplicatedIdents] + 0.001*duplicatedIdents
      }
    }
    
    # Can't do projection for alaska or hawaii
    if(!stateUse %in% c("Alaska","Hawaii")){
    
      # Project the state map
      statesLimits <- map_data("state")
      statesLimits = statesLimits[which(statesLimits$region == tolower(stateUse)),]
      minLat = min(statesLimits$lat); maxLat = max(statesLimits$lat); minLon = min(statesLimits$long); maxLon = max(statesLimits$long) 
      midLat = (maxLat + minLat)/2; midLon = (maxLon + minLon)/2
      
      stateSubset <- stateSubset %>%
        st_transform(crs = paste0("+proj=aea +lat_1=",minLat," +lat_2=",maxLat," +lat_0=",midLat," +lon_0=",midLon," +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
    }
    
    #use previous maps package
    
    # By date
    #Silence the aesthetics we use for plotly but not directly in ggplot
    suppressWarnings(
      p3 <- ggplot() +
        geom_sf(data = stateSubset, aes(fill = value, State = State, County = County, cases = cases, deaths = deaths, casesPerMillion = casesPerMillion, deathsPerMillion = deathsPerMillion,
                                        text = paste0("State: ", str_to_title(State), "</br></br>County: ", str_to_title(County),
                                                      "</br>Cases: ", format(cases,big.mark=",",scientific=FALSE),
                                                      "</br>Deaths: ", format(deaths,big.mark=",",scientific=FALSE), "</br>Cases Per Million: ", format(round(casesPerMillion),big.mark=",",scientific=FALSE),
                                                      "</br>Deaths Per Million: ", format(round(deathsPerMillion),big.mark=",",scientific=FALSE))),
                color = "gray50", size = 0.5) +
        geom_sf(data = stateSubset, aes(color = as.numeric(county_fips), State = State, County = County, cases = cases, deaths = deaths, casesPerMillion = casesPerMillion, deathsPerMillion = deathsPerMillion,
                                        NewCasesPerMillion = NewCasesPerMillion,  NewCases =  NewCases, NewDeathsPerMillion = NewDeathsPerMillion,  NewDeaths =  NewDeaths,
                                        
                                        text = paste0("State: ", str_to_title(State), "</br></br>County: ", str_to_title(County), 
                                                      "</br>Cases (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(cases,big.mark=",",scientific=FALSE),
                                                      "</br>Deaths (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(deaths,big.mark=",",scientific=FALSE),
                                                      "</br>Cases Per Million (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(round(casesPerMillion),big.mark=",",scientific=FALSE),
                                                      "</br>Deaths Per Million (",format(input$daterange1[1], "%m/%d")," - ",format(input$daterange1[2], "%m/%d"),"): ", format(round(deathsPerMillion),big.mark=",",scientific=FALSE),
                                                      "</br>New Cases (",format(input$daterange1[2], "%m/%d"),"): ", format(NewCases,big.mark=",",scientific=FALSE),
                                                      "</br>New Deaths (",format(input$daterange1[2], "%m/%d"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE),
                                                      "</br>New Cases Per Million (",format(input$daterange1[2], "%m/%d"),"): ", format(NewCasesPerMillion,big.mark=",",scientific=FALSE),
                                                      "</br>New Deaths Per Million (",format(input$daterange1[2], "%m/%d"),"): ", format(NewDeathsPerMillion,big.mark=",",scientific=FALSE)
                                        )),
                size = 0.01,alpha = 1,fill=NA) +
        theme_bw() +
        # scale_fill_gradient2(midpoint = median(stateSubset$value[!is.na(stateSubset$value)]), low = "#0571b0", mid = "#f7f7f7", high = "#ca0020", na.value="gray75") +
        scale_fill_gradient2(midpoint = median(stateSubset$value[!is.na(stateSubset$value)]), low = "white", mid = "#ffbe87", high = "#e6550d", na.value="gray75") +
        labs( fill = "Percentile" ) + #edit the legend title
        ggtitle(annoTitle) +
        theme(
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none") #remove the legend
    )
    ggplotly(p3, tooltip = "text") %>% style(hoveron = "key") #%>%
      # partial_bundle(local = FALSE) %>%
      # plotly_mod_dep() #%>%
      # toWebGL()
    # toWebGL(p3)
    
  })
}

######################################################################
#    Run App  
######################################################################

# new -- to update the url
#options(shiny.port = 8888)
#options(shiny.host = "0.0.0.0")

enableBookmarking("url")
shinyApp(ui = ui, server = server)