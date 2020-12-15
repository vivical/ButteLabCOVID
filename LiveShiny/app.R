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
library(roll)
library(zoo)
devtools::install_github("UrbanInstitute/urbnmapr") # Uncomment
library(urbnmapr) #devtools::install_github("UrbanInstitute/urbnmapr")
library(USAboundaries) # to get state boundaries
# library(mapproj) albers projection? -- seems non-trivial

# New Libraries 8-20-2020
library(DT)
library(shinyjs) # requires shinyjs !

# Load Elliott Libraries
library(plotly)
library(ggplot2)
load(file = "./DataFiles/CovidCountiesWorkspace.RData")
# Get a table for ICU
tableDat <- tibble(State = countyDat$state, County = countyDat$county, Date = countyDat$date, "Estimated ICU Beds Needed" = countyDat$icu_bed_occ, 
                   "ICU Beds Available" = countyDat$ICUbeds, "Estimated % ICU Beds Needed" = countyDat$perc_icu_occ/100, 
                   "80% CI" = paste0(round(countyDat$perc_icu_occ - 0.585*countyDat$perc_icu_occ,digits = 2), "-", 
                                                   round(countyDat$perc_icu_occ + 0.585*countyDat$perc_icu_occ,digits = 2),"%"))
plotTableDat <- tableDat

# Table for hospitalization and ICU parameters
parameterTableDat <- tibble(`Date Updated` = as.Date(c("2020-12-15","2020-11-16","2020-10-15","2020-09-15","2020-08-15","2020-06-10")), `Hospitalization Rate` = c(0.0443,0.0502,0.0509,0.0558,0.0826,0.1270), `ICU Rate` = c(0.2013,0.2185,0.2607,0.22908,0.2964,0.4000),
                            `Parameter Estimation Range Start` = as.Date(c("2020-10-24","2020-09-26","2020-08-22","2020-07-25","2020-06-28","2020-03-07")), `Parameter Estimation Range End` = as.Date(c("2020-12-05","2020-11-07","2020-10-03","2020-09-04","2020-08-08","2020-05-30")))
# 12-15-2020

######
# Add state data to the county data
stateDatAdd <- tibble(date = stateDat$date, county = paste0(stateDat$state," State"), state = stateDat$state, fips = NA, cases = stateDat$cases, deaths = stateDat$deaths,
                      NewCases = stateDat$NewCases, NewDeaths = stateDat$NewDeaths, NAME =  paste0(stateDat$state," State"), variable = "POP", value = stateDat$value,
                      casesPerMillion = stateDat$casesPerMillion, deathsPerMillion = stateDat$deathsPerMillion, NewCasesPerMillion = stateDat$NewCasesPerMillion,
                      NewDeathsPerMillion = stateDat$NewDeathsPerMillion, double = stateDat$double, ICUbeds = stateDat$ICUbeds,
                      icu_bed_occ = stateDat$icu_bed_occ, tot_icu_bed_occ = stateDat$total_icu_bed_occ, perc_icu_occ = stateDat$perc_icu_occ,
                      loessN = stateDat$loessN)

suppressWarnings( # this is noisy for N/As
  time_zero_state <- stateDatAdd %>%
    group_by(state) %>%
    dplyr::summarise(first_case = min(date[which(cases>=10)])) %>%
    ungroup
)
stateDatAdd <- stateDatAdd %>%
  left_join(time_zero_state, by = c("state")) %>%
  dplyr::mutate(time = as.numeric(date - first_case))

countyDat <- rbind(countyDat,stateDatAdd)
######

# countyDat$perc_icu_occ <- countyDat$perc_icu_occ/100 #08-14-2020
# load(file = "/home/oskotsky/PycharmProjects/covidcounties/DataFiles/CovidCountiesWorkspace.RData")
addResourcePath("www", paste(getwd() , "/www", sep="") )

# timeout 
timeoutSeconds <- 600

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

######################################################################
#  UI 
######################################################################
######################################################################

#dim(stateDat)
#  Original State Data dimensions: 1437    5
# Formatted State Data dimensions: 1437    5
#  Original County Data dimensions: 17731     6
# Formatted County Data dimensions: 17458    18


ui = fluidPage( style='margin-left:5px; margin-right:5px', title="COVID-19 County Tracker",
                useShinyjs(), #8-20-2020 allows us to show and hide table with confidence intervals
                tags$script(inactivity), 
                tags$head(includeHTML("www/google_analytics.html")), # Add google analytics for tracking site
                tags$head(tags$link(rel="shortcut icon", href="www/virus2_icon.svg")),  # Icon to display in browser tab
                
                
                ######################################################################
                #  Title Section
                ######################################################################
                
                #div( class="jumbotron", style="background-color: white; padding-left:.4em; padding-right:.4em; padding-bottom:.1em; padding-top: 1.3em", 
                #     
                #     p(tags$small(" .")),
                #     div( #class="col-xs-10 col-md-11 col-lg-11",
                #       h3( class="text-center", style="padding-top:0px;margin-top:0px", "COVID-19 County Tracker") #, HTML("<i><font color='Crimson'>Tracker</font></i>") ) #,
                #     ),
                #     fluidRow(div( class="col-xs-12 col-md-offset-2 col-md-8 col-lg-offset-3 col-lg-6",
                #                   HTML("<hr style='padding:0px; margin:0px'>")
                #     )),  
                #     
                #     h4(class="text-center", "COVID-19 cases across all 3,142 counties in the United States" ),
                #     p( tags$small( class="text-center", style="padding-bottom:0px; margin-bottom:0px", 
                #                    "COVID-19 is rapidly spreading across the United States, but at different rates and intensities.  Medical professionals, leaders, and policy makers should have the most up-to-date information about cases and growth trends in their local region.  The ",
                #                    a(href="http://buttelab.ucsf.edu/", "Butte Lab",inline = T, target = "_blank"), " at UCSF has partnered with ", 
                #                    a(href="https://twitter.com/pbleic?s=20", "Paul Bleicher",inline = T, target = "_blank"),
                #                    "MD, PhD, former CEO of OptumLabs, to calculate and visualize COVID-19 statistics for every county in the United States. Use the tools below to view the most recent data for any region. For questons and feedback, please ",
                #                    a(href="mailto:covid.tracker@bakar.institute", "contact us.",inline = T, target = "_blank") ) )
                #),
                
                div( class="container-fluid", #class="jumbotron", style="background-color: white; padding-left:.4em; padding-right:.4em; padding-bottom:.1em; padding-top: 1.3em", 
                     
                     p(tags$small(" .")),
                     h2( class="text-center", "COVID-19 County Tracker"), 
                     fluidRow(div( class="col-xs-12 col-md-offset-2 col-md-8 col-lg-offset-3 col-lg-6",
                                   HTML("<hr style='padding:0px; margin:0px'>")
                     )),  
                     h4(class="text-center", "COVID-19 cases across all 3,142 counties in the United States" ),
                     p( class="text-center", style="font-size:1.1em; max-width: 1000px; margin: auto", 
                        "COVID-19 is rapidly spreading across the United States, but at different rates and intensities.  Medical professionals, leaders, and policy makers should have the most up-to-date information about cases and growth trends in their local region.  The ",
                        a(href="http://buttelab.ucsf.edu/", "Butte Lab",inline = T, target = "_blank"), " at UCSF has partnered with ", 
                        a(href="https://twitter.com/pbleic?s=20", "Paul Bleicher",inline = T, target = "_blank"),
                        "MD, PhD, former CEO of OptumLabs, to calculate and visualize COVID-19 statistics for every county in the United States. Use the tools below to view the most recent data for any region. This project is open source and we welcome ", 
                        a(href="www/volunteers.html", "volunteers (both technical and non-technical)",inline = T, target = "_blank"), " to help keep our resource up to date. For questons and feedback, please ",
                        a(href="mailto:covid.tracker@bakar.institute", "contact us.",inline = T, target = "_blank") )#,
                     #p(tags$small(" .")) # vertical white space
                ),
                
                ######################################################################
                #  Sidebar
                ######################################################################
                div( class="container", #style="background-color:#F2F3F4;",
                     div(class="col-xs-12", align="center",  
                         tags$span( selectizeInput("cState", "Select a state:",
                                                   selected = defaultState, # default value
                                                   choices = c(sort(as.character(unique(stateDat$state))))),
                                    HTML('<button type="button" class="btn btn-default btn-sm" data-toggle="collapse" data-target="#demo">Customization Options</button>')   # style="width: 200px ; height: 50px; font-size: 125%"
                         )          
                     ),
                     
                     div( id="demo", class="collapse",
                          fluidRow( column(12, tags$hr() )),
                          #tags$hr(),
                          fluidRow(
                            div( class="col-sm-3 col-xs-6", radioButtons("uPlot", "Plot by:",
                                                                         selected = "cases", # default value
                                                                         choices = c("Total cases" = "cases",
                                                                                     "Total deaths" = "deaths",
                                                                                     "New cases" = "nCases",
                                                                                     "New deaths" = "nDeaths",
                                                                                     "Doubling time"='loessN',
                                                                                     "Estimated % ICU beds needed"='ICUbeds'))),#4-7-2020 #TRAVIS ZACK added this for doubling option
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
                            column( 4,  
                                    numericInput("mCases", "Include counties with more cases than:", 
                                                 value = 25,# default value
                                                 min = 0, 
                                                 max = max(countyDat$cases))#,
                                    # p("View/hide a county by clicking its name. Double-click to select only one county.", style="padding-top:0; margin-top:0"),
                            ),
                            column( 4,  
                                    radioButtons("dArrows", "Show State Mandates:",
                                                 selected = "false", # default value
                                                 choices = c("True" = "true",
                                                             "False" = "false"))
                            ),
                            column(4,
                                   # 6-23-2020
                                   shinyalert::useShinyalert(),
                                   dateRangeInput("daterange1", paste0("Date Range (data is available from: 2020-01-21 to ",as.Date((max(countyDat$date) - 1)),"):"), # (includes data from 1/1/20-Present)
                                                  start  = as.Date("2020-03-01"), # default value
                                                  # start  = base::as.Date("2020-03-01", format='%Y-%m-%d'), #6-19-2020 v1
                                                  # start  = as.POSIXlt("2020-03-01", tz = 'America/Los_Angeles'), #6-19-2020 v2
                                                  # start  = base::as.Date("2020-03-01"), #6-19-2020 v3
                                                  # end    = max(countyDat$date),  # default value
                                                  end    = (max(countyDat$date) - 1),  #6-22-2020 v1  -- need to set the start to less than max value for EU
                                                  # end    = (max(countyDat$date) - 2),  #6-22-2020 v2  -- need to set the start to less than max value for EU
                                                  # end    = max(base::as.Date(countyDat$date,format='%Y-%m-%d')), #6-19-2020 v1
                                                  # end    = max(as.POSIXlt(as.character(pull(countyDat[,"date"])), tz = 'America/Los_Angeles')), #6-19-2020 v2
                                                  # end  = base::as.Date(maxDate), #6-19-2020 v3
                                                  min    = min(countyDat$date),  # default value
                                                  # min    = min(base::as.Date(countyDat$date,format='%Y-%m-%d')), #6-19-2020 v1
                                                  # min    = min(as.POSIXlt(as.character(pull(countyDat[,"date"])), tz = 'America/Los_Angeles')), #6-19-2020 v2
                                                  # min  = base::as.Date(minDate), #6-19-2020 v3
                                                  # max    = max(countyDat$date),  # default value
                                                  max    = max(countyDat$date),  # 6-22-2020 v1
                                                  # max    = (max(countyDat$date) - 1),  # 6-22-2020 v2
                                                  # max    = max(base::as.Date(countyDat$date,format='%Y-%m-%d')), #6-19-2020 v1
                                                  # max    = max(as.POSIXlt(as.character(pull(countyDat[,"date"])), tz = 'America/Los_Angeles')), #6-19-2020 v2
                                                  # max  = base::as.Date(maxDate), #6-19-2020 v3
                                                  # format = "mm/dd/yy",
                                                  format = "yyyy-mm-dd",
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
                     h2(class="text-center", textOutput("state_title",inline = T) ),
                     h4(class="text-center", textOutput("main_plot_title",inline = T) ),
                     p(class="text-center",'View/hide a county by clicking its name in the legend. Double-click to select only one county.'),
                     div( plotlyOutput("stateLinePlot"), style="border: 1px solid gray; height: 525px; max-width: 1000px; margin: auto" ), #4-8-2020 modified height to make legend better for states with many counties (e.g. CA)
                     p(class="text-center", textOutput("doubling_time", inline = T) ),
                     div( class="col-xs-5", align="right", 
                          h5("Select Counties:", style="padding-top: 1.1em")
                     ),
                     div( class="col-xs-7",# align="center", 
                          #("Select County: " ,
                          # uiOutput("secondSelection")
                          
                          selectizeInput( "cCounty", " ",
                                          selected = default_all_label,
                                          choices = c(default_all_label, default_state_counties_names,default_rest_counties_names), #4-8-2018
                                          multiple = TRUE
                          )
                          
                          #selectInput( "cCounty", " ",
                          #             selected = "All", # default value
                          #             choices = c("1m","2,","3","All") )
                     ),
                     fluidRow( div(class="col-xs-offset-5 col-xs-7",
                                   p("Search any US county. Select/Delete to remove.", style="margin-top:0px; margin-bottom:0px; padding-top:0px; padding-bottom:0px") 
                     ) ),
                     p( style="margin-bottom:0px; padding-bottom:0px", tags$small("."))
                     
                     #plotlyOutput("stateLinePlot")
                ), # ends "mainPanel" from "Main Plot" section
                #fluidRow(p(tags$small(".")) ), #creates a tiny amount of vertical space
                
                ######################################################################
                #  Confidence Interval Table
                ######################################################################
                div( class="container-fluid",
                     div(DT::dataTableOutput("CITable"), style="max-width: 1000px; margin: auto"), #8-20-2020
                     p( style="margin-bottom:0px; padding-bottom:0px; max-width: 1000px", tags$small("."))
                ), # ends "CITable" from "Confidence Interval Table" section
                #fluidRow(p(tags$small(".")) ), #creates a tiny amount of vertical space
                
                ######################################################################
                #  US Map and State Map
                ######################################################################
                fluidRow(
                  
                  div( class="col-xs-12 col-md-6",
                       h4(class="text-center", textOutput("state_map_title",inline = T) ),
                       p(class="text-center", style="padding-bottom:0px; margin-bottom:0px",
                         #a(href="https://storage.googleapis.com/bakar-data/covid_tracker/Screen%20Shot%202020-04-07%20at%203.26.31%20AM.png", "Percentile ranks ",inline = T, target = "_blank"),
                         a(href="www/percentile_ranks.png", "Percentile ranks ",inline = T, target = "_blank"),
                         "are relative to counties within the state"),
                       p(class="text-center", style="padding-top:0px; margin-top:0px", tags$small( textOutput("percentile_info",inline = T) )),
                       div( plotlyOutput("stateMapPlot"), style="border: 1px solid gray; max-width: 650px; margin: auto" ),
                       p(class="text-center", style="padding-bottom:0px; margin-bottom:0px", tags$small(   'Click and drag in plot to zoom selected region')),
                       p(class="text-center", style="padding-bottom:0px; margin-bottom:0px", tags$small(   'Click "reset axes" in the upper-right to reset the plot')),
                       p( tags$small( '.')) # this is here just for vertical space
                  ),
                  div( class="col-xs-12 col-md-6", 
                       h4(class="text-center", textOutput("us_map_title",inline = T) ),
                       #p(class="text-center", textOutput("us_map_subtitle",inline = T) ), #4-7-2-20
                       p(class="text-center", style="padding-bottom:0px; margin-bottom:0px", "Select a state mandate below to compare start dates"),
                       p(class="text-center", style="padding-top:0px; margin-top:0px", tags$small( "Default shows data in graph title; ", textOutput("percentile_info2",inline = T) )),
                       div( plotlyOutput("usaPlot"), style="border: 1px solid gray; max-width: 650px; margin: auto" ),
                       #p(class="text-center", style="padding-bottom:0px; margin-bottom:0px", tags$small(   'Click and drag in plot to zoom selected region')),
                       #p(class="text-center", style="padding-bottom:0px; margin-bottom:0px", tags$small(   'Click "reset axes" in the upper-right to reset the plot')),
                       #p( tags$small( '.')) # this is here just for vertical space
                       div(  align="center",                   
                             selectInput( "uEvents",NULL, selected = "default", # default value,
                                          choices=c("Default" = "default",
                                                    "State of Emergency" = "soe",
                                                    #"Social Distancing" = "sd", #4-7-2-20
                                                    "Closure of Public Schools" = "cs",
                                                    "Shelter-In-Place Instituted" = "sip",
                                                    "Closure of Restaurants/Bars" = "rb")) )
                  )
                  
                ) ,
                
                ######################################################################
                #  Table
                ######################################################################
                # div( class="container-fluid",
                #      h2(class="text-center", textOutput("state_title",inline = T) ),
                #      # h4(class="text-center", textOutput("main_plot_title",inline = T) ),
                #      # p(class="text-center",'View/hide a county by clicking its name in the legend. Double-click to select only one county.'),
                #      # div( plotlyOutput("stateLinePlot"), style="border: 1px solid gray; height: 525px; max-width: 1000px; margin: auto" ), #4-8-2020 modified height to make legend better for states with many counties (e.g. CA)
                #      div( dataTableOutput('countyTable'), style="border: 1px solid gray; height: 525px; max-width: 1000px; margin: auto" ),
                #      # p(class="text-center", textOutput("doubling_time", inline = T) ),
                #      p( style="margin-bottom:0px; padding-bottom:0px", tags$small("."))
                #      
                # ), # ends "countyTable" from "Table" section
                
                ######################################################################
                #  Select State Mandated Events to Plot
                ######################################################################
                #fluidRow(
                
                #),
                
                #fluidRow(
                #  div( class="col-sm-3 col-xs-6", radioButtons("uDates", "Replace COVID-19 Realted Counts With State Mandated Event Dates on US State Map:",
                #                                               selected = "false", # default value
                #                                               choices = c("True" = "true",
                #                                                           "False" = "false"))),
                #  div( class="col-sm-3 col-xs-6", radioButtons("uEvents", "Select State Mandated Event:",
                #                                               selected = "sd", # default value
                #                                               choices = c("State of Emergency" = "soe",
                #                                                           "Social Distancing" = "sd",
                #                                                           "Closure of Public Schools" = "cs",
                #                                                           "Shelter-In-Place Instituted" = "sip",
                #                                                           "Closure of Restaurants/Bars" = "rb"))),
                #  div( class="col-sm-3 col-xs-6"),
                #  div( class="col-sm-3 col-xs-6")
                #),
                
                ######################################################################
                #  Hospitalization and ICU Parameter Table
                ######################################################################
                div( class="container-fluid",
                     p( class="text-center", style="font-size:1.1em; max-width: 1000px; margin: auto", 
                        "The COVID-19 testing and hospitalization landscape is constantly evolving. In order to keep our estimated % ICU beds needed model accurate and 
                        up to date, we will periodically update the underlying parameters. Changing these parameters impacts model performance for dates prior to the range for 
                        which the parameters are estimated. However, we believe that it is most imporantant from a policymaking standpoint to understand the current demands on 
                        healthcare resources and thus compromise on accuracy for the earlier dates of the pandemic. To keep a record of how these parameters have changed 
                        over the course of the pandemic and inform on the accuracy of the earlier predictions, we provide a history of the parameters used on covidcounties. 
                        The parameters currently being used are highlighted in yellow."
                        ),
                     div(DT::dataTableOutput("ParameterTable"), style="max-width: 1000px; margin: auto"), #9-15-2020
                     p( style="margin-bottom:0px; padding-bottom:0px; max-width: 1000px", tags$small("."))
                ), # ends "ParameterTable"
                #fluidRow(p(tags$small(".")) ), #creates a tiny amount of vertical space
                
                ######################################################################
                #  Footer Info
                ######################################################################
                
                
                # div( class="jumbotron", style="background-color: white; padding: .4em",
                
                div(class = "container", sytle="max-width: 2000px; margin: 0 auto; display: block;",
                
                # Video Tutorial
                # HTML( "<div style='max-width: 700px; margin: auto;' class='col-xs-offset-0 col-xs-12 col-sm-offset-0 col-sm-6 col-md-6 col-lg-5'>
                HTML( "<div style='max-width: 700px; margin: 0 auto; display: block;' class='col-xs-12 col-md-6 col-lg-6'>
                        <h3 class='text-center'>Tutorial</h3>
                        <div class='embed-responsive embed-responsive-16by9'>
                          <iframe style='max-width: 650px; margin: auto;' class='embed-responsive-item'
                          src='https://www.youtube.com/embed/5OHDSpLv1kY'></iframe>
                          </div>
                        </div>"),
                
                # div( style='max-width: 700px; margin: auto;', class='col-xs-12 col-sm-6 col-md-6 col-lg-6',
                div( style='max-width: 700px; margin: 0 auto; display: block;', class='col-xs-12 col-md-6 col-lg-6',
                      ### Data Sources
                      h3( class="text-center", "Acknowledgements" ), # class="text-center",
                      p(class="text-left",  "State and county infection data: ",
                        a(href="https://github.com/nytimes/covid-19-data", "New York Times COVID-19",inline = T, target = "_blank"), "github."),
                      p(class="text-left",  "US Census data: ",
                        a(href="https://www.census.gov/developers/", "US Census API",inline = T, target = "_blank"),
                        "and the",
                        a(href="https://walkerke.github.io/tidycensus/", "tidycensus",inline = T, target = "_blank"),
                        "R package."),
                      p( "ICU bed counts: ",
                         a(href="https://khn.org/news/as-coronavirus-spreads-widely-millions-of-older-americans-live-in-counties-with-no-icu-beds/", "Kaiser Health News Data",inline = T, target = "_blank") 
                      ),
                      p( class="text-left", "R Packages: ",
                         a(href="https://shiny.rstudio.com", "shiny, ",inline = T, target = "_blank"),
                         a(href="https://deanattali.com/shinyjs", "shinyjs, ",inline = T, target = "_blank"),
                         a(href="https://www.tidyverse.org/", "tidyverse, ",inline = T, target = "_blank"),
                         a(href="https://plotly.com/r/", "plotly, ",inline = T, target = "_blank"), "& ",
                         a(href="https://walkerke.github.io/tidycensus/", "tidycensus",inline = T, target = "_blank")
                      ),
                      p( "Web hosting: Generously supported by ",
                         a(href="https://aws.amazon.com/", "Amazon Web Services",inline = T, target = "_blank")
                      ),
                      
                      #class="col-xs-12 col-sm-7 col-md-5",
                      fluidRow( p(  style="padding-left:.8em;", strong("Team: "), # style="padding-left:0px",  margin-left:0px
                                    a(href="mailto:douglas.arneson@ucsf.edu","Douglas Arneson, ",inline = T, target = "_blank"),
                                    a(href="https://twitter.com/pbleic?s=20","Paul Bleicher, ",inline = T, target = "_blank"),
                                    a(href="https://twitter.com/atulbutte?s=20","Atul Butte, ",inline = T, target = "_blank"),
                                    a(href="mailto:matthew.elliott@ucsf.edu","Matthew Elliott, ",inline = T, target = "_blank"),
                                    a(href="https://twitter.com/armanmosenia?s=20","Arman Mosenia, ",inline = T, target = "_blank"),
                                    a(href="mailto:boris.oskotsky@ucsf.edu","Boris Oskotsky, ",inline = T, target = "_blank"),
                                    a(href="https://twitter.com/vivicality?s=20","Vivek Rudrapatna, ",inline = T, target = "_blank"),
                                    a(href="https://twitter.com/vashishtrv?s=20","Rohit Vashisht, ",inline = T, target = "_blank"),
                                    a(href="mailto:travis.zack@ucsf.edu","Travis Zack",inline = T, target = "_blank")
                      ))
                ),
                
                # div(style='max-width: 700px; margin: auto;', class="col-xs-12 col-sm-6 col-md-6 col-lg-6",
                div(style='max-width: 700px; margin: 0 auto; display: block;', class="col-xs-12 col-md-6 col-lg-6",
                     img(src='www/bakar_logo.png', class="img-responsive")
                ),
                #),
                ),

                fluidRow(class="jumbotron", style="background-color: white", HTML("<h1>&nbsp</h1>") ),
                
                ######################################################################
                #  Loadingg Message
                ######################################################################
                # Loading message test
                tags$style( type="text/css", "
                             #loadmessage { position: fixed; top: 0px; left: 0px; width: 100%; padding: 5px 0px 5px 0px;
                               text-align: center; font-weight: bold; font-size: 100%; color: #1B2631;
                               background-color: #FFCED4; z-index: 105;  }
                  "),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("We'll be right with you",id="loadmessage")
                )
)



######################################################################
######################################################################
#    Server 
######################################################################
######################################################################

### Timeout

server = function(input, output, session) {
  
  # Data table for ICU and hospitalization parmaters 9-15-2020
  output$ParameterTable <- DT::renderDataTable({
    datatable(parameterTableDat, filter = 'none', #remove column filter
              options = list(
                pageLength = 5,
                dom = 't', #remove searchbar
                autoWidth = FALSE),
              rownames = FALSE
    )%>% 
      formatPercentage(c("Hospitalization Rate","ICU Rate"), 2) %>%
      formatStyle(
        'Date Updated',
        target = 'row',
        backgroundColor = styleEqual(as.Date('2020-12-15'), c('yellow')) #12-15-2020
      )
  })
  
  # Data table for confidence interval 8-20-2020
  observeEvent(input$uPlot, {
    if (input$uPlot == "ICUbeds"){
      plotTableDat <- tableDat[which(tableDat$State == input$cState),]
      output$CITable <- DT::renderDataTable({
        datatable(plotTableDat, filter = 'top', 
                  options = list(
                    pageLength = 5, 
                    autoWidth = FALSE,
                    order = list(list(2, 'desc'), list(5, 'desc'))),
                  rownames= FALSE
        ) %>% formatRound(columns=c('Estimated ICU Beds Needed'), digits=2) %>%
          formatPercentage(c("Estimated % ICU Beds Needed"), 2)
      })
      show("CITable")
      } else {
        hide("CITable")
      }
  })
  observeEvent(input$cState, {
    if (input$uPlot == "ICUbeds"){
      plotTableDat <- tableDat[which(tableDat$State == input$cState),]
      output$CITable <- DT::renderDataTable({
        datatable(plotTableDat, filter = 'top', 
                  options = list(
                    pageLength = 5, 
                    autoWidth = FALSE,
                    order = list(list(2, 'desc'), list(5, 'desc'))),
                  rownames= FALSE
        ) %>% formatRound(columns=c('Estimated ICU Beds Needed'), digits=2) %>%
          formatPercentage(c("Estimated % ICU Beds Needed"), 2)
      })
      show("CITable")
    } else {
      hide("CITable")
    }
  })
  output$CITable <- DT::renderDataTable({
    datatable(plotTableDat, filter = 'top', 
              options = list(
                pageLength = 5, 
                autoWidth = FALSE,
                order = list(list(2, 'desc'),list(5, 'desc'))),
              rownames= FALSE
    ) %>% formatRound(columns=c('Estimated ICU Beds Needed'), digits=2) %>%
      formatPercentage(c("Estimated % ICU Beds Needed"), 2)
  })
  
  #########
  
  observeEvent(input$timeOut, { 
    print(paste0("Session (", session$token, ") timed out at: ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
      footer = NULL
    ))
    session$close()
  })
  
  # Handle when max date value is selected: 6-23-2020
  r <- reactiveValues(
    start = as.Date("2020-03-01"),
    end = (max(countyDat$date) - 1)
  )
  # Handle when max date value is selected: 6-23-2020
  observeEvent( input$daterange1 , {
    start <- input$daterange1[[1]]
    end <- input$daterange1[[2]]
    if (end == max(countyDat$date)){
      shinyalert::shinyalert("end date selected is beyond available data", type = "error")
      updateDateRangeInput(
        session, 
        "daterange1", 
        start = r$start,
        end = r$end
      )
    } else {
      r$start <- input$daterange1[[1]]
      r$end <- input$daterange1[[2]]
    }
  }, ignoreInit = TRUE)
  
  #points <- eventReactive(input$recalc, {
  #  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  #}, ignoreNULL = FALSE)
  
  #output$mymap <- renderLeaflet({
  #  leaflet() %>%
  #    addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>% 
  #    addMarkers(data = points())
  #})
  
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
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if(!is.null(query[['st']])) updateSelectizeInput(session, "cState",selected = query[['st']])
  #   if(!is.null(query[['pb']])) updateRadioButtons(session, "uPlot",selected = query[['pb']])
  #   if(!is.null(query[['ts']])) updateRadioButtons(session, "uTime",selected = query[['ts']])
  #   if(!is.null(query[['sc']])) updateRadioButtons(session, "uScale",selected = query[['sc']])
  #   if(!is.null(query[['ax']])) updateRadioButtons(session, "pScale",selected = query[['ax']])
  #   if(!is.null(query[['min']])) updateNumericInput(session, "mCases",value = query[['min']])
  #   if(!is.null(query[['dr1']])) updateDateRangeInput(session, "daterange1",start = query[['dr1']])
  #   if(!is.null(query[['dr2']])) updateDateRangeInput(session, "daterange1",end = query[['dr2']])
  # })
  # 
  # ######################################################################
  # #  Add parameters to the URL
  # ######################################################################
  # observeEvent(c(input$cState,input$uPlot,input$uTime,input$uScale,input$pScale,input$mCases,input$daterange1), {
  #   updateQueryString(paste0("?st=",input$cState,"&pb=",input$uPlot,"&ts=",input$uTime,"&sc=",input$uScale,"&ax=",input$pScale,"&min=",input$mCases,"&dr1=",input$daterange1[1],
  #                            "&dr2=",input$daterange1[2]), mode = "replace")
  # })
  
  ######################################################################
  #  Add Counties to County Selector Input
  ######################################################################
  
  observeEvent(input$cState,{
    
    state_counties <- sort((countyDat %>% filter(state==input$cState) %>% distinct(county) )$county ) #4-8-2018
    state_counties_names <- paste0(input$cState,": ", state_counties) #4-8-2018
    rest_counties <- unique(paste0(countyDat$state,": ", countyDat$county)) #4-8-2018
    rest_counties_names <- sort(setdiff(rest_counties,state_counties_names)) #4-8-2018
    # default_all_label <- "All California"
    all_label <- paste("All",input$cState)
    
    updateSelectizeInput(session,  "cCounty", " ",
                         selected = all_label,
                         choices = c(all_label, state_counties_names,rest_counties_names)
    )
    
  })
  
  
  # output$secondSelection <- renderUI({
  #   # Added Doug -- more flexible input (autofill and select multiple)
  #     state_counties <- sort((countyDat %>% filter(state==input$cState) %>% distinct(county) )$county ) #4-8-2018
  #     state_counties_names <- paste0(input$cState,": ", state_counties) #4-8-2018
  #     rest_counties <- unique(paste0(countyDat$state,": ", countyDat$county)) #4-8-2018
  #     rest_counties_names <- sort(setdiff(rest_counties,state_counties_names)) #4-8-2018
  #     # default_all_label <- "All California"
  #     all_label <- paste("All",input$cState)
  #     selectizeInput( "cCounty", " ",
  #                     # selectInput( "cCounty", " ",
  #                     # selected = default_all_label, #asdf1
  #                     selected = all_label, #asdf1
  #                     #THIS WAS THE ORIGINAL -- TRAVIS ZACK
  #                     #choices = c("All", sort((countyDat %>% filter(state==input$cState) %>% distinct(county) )$county )),
  #                     #THIS IS THE REPLACEMENT "choices" -- TRAVIS ZACK
  #                     choices = c(all_label, state_counties_names,rest_counties_names), #4-8-2018
  #                     multiple = TRUE
  #     )
  # })
  
  ######################################################################
  #  Quickly and efficiently filter data for the line plot
  ######################################################################
  
  ######################################################################
  #  Line plot filter functions
  ######################################################################
  
  # Filter by the minimum number of cases to include the county
  filterMinCases <- reactive({
    # Pull in the county data
    req(countyDat)
    
    # Handle when max date value is selected: 6-23-2020
    validate(
      need(input$daterange1[[2]] != max(countyDat$date), 'end date selected is beyond available data.')
    )
    
    # This filters by the minimum number of cases
    # This code is ignored when the user looks at specific counties
    max_cases = if( "All" %in% input$cCounty ) input$mCases else 0
    selected_counties <- countyDat %>%
      group_by(state, county) %>%
      summarise(max_cases_per_county = max(cases, na.rm = T)) %>%
      mutate(has_enough_cases = (max_cases_per_county > max_cases)) %>%
      filter(has_enough_cases) %>%
      ungroup
    
    dat <- countyDat %>%
      left_join(selected_counties, by = c("state", "county")) %>%
      ungroup
    
    dat
  })
  
  # To add counties from other states, we do not filter by the state upfront
  # # Filter by the state
  # filterState <- reactive({
  #   filterMinCases() %>% 
  #     filter(state == input$cState)
  # })
  
  # Filter by the date range for all states
  filterDateRange <- reactive({
    # dat <- filterState() %>% 
    dat <- filterMinCases() %>% 
      filter(date >= input$daterange1[1] &
               date <= input$daterange1[2])
    # Add exception if no state is selected
    validate(
      need(nrow(dat) >0, 'Please choose a date range for the state which has available data.')
    )
    dat
  })
  
  #Filter by the date range for the selected state
  filterDateRangeState <- reactive({
    dat <- filterMinCases() %>% 
      filter(state == input$cState & 
               date >= input$daterange1[1] &
               date <= input$daterange1[2])
    # Add exception if no state is selected
    validate(
      need(nrow(dat) >0, 'Please choose a date range for the state which has available data.')
    )
    dat
  })
  
  
  # Ignored when looking at specific cases
  # # Filter by the number of cases
  # filterNumCases <- reactive({
  #   filterDateRange() %>%
  #     filter(has_enough_cases)
  # })
  
  
  # # Use cCounty input to select a single county when "All" is not selected
  # filterCounty <- reactive({
  #   dat <- filterNumCases()
  #   if(length(input$cCounty) != 0){
  #     if(!"All" %in% input$cCounty){
  #       dat <- dat %>% filter(county %in% input$cCounty)
  #     }
  #   }
  #   dat
  # })
  
  filterCounty <- reactive({
    dat1 <- filterDateRange()
    # exception if there is no filtering
    dat <- filterDateRangeState()
    if(length(input$cCounty) != 0){ # Added by Doug -- initially was throwing an error when the app loaded (b/c there is no value)
      if(!c(paste("All",input$cState)) %in% input$cCounty){
        # Get the counties from the correct state
        dat <- dat1 %>% #4-8-2020
          mutate(StateCounty = paste0(state,": ",county)) %>% #4-8-2020
          filter(StateCounty %in% input$cCounty) #4-8-2020
        dat <- unique(dat)
      } else if ((length(input$cCounty)==1) & (input$cCounty[1]== paste("All",input$cState))) {
        dat <- dat1 %>% filter(state == input$cState )
      } else {
        just_counties = input$cCounty[!input$cCounty %in% c(paste("All",input$cState))]
        dat2 <- dat %>% #4-8-2020 -- need to add the mutate to make the col numbers match
          filter(state == input$cState ) %>%
          mutate(StateCounty = paste0(state,": ",county)) #4-8-2020
        # dat3 <- dat1 %>% filter(county %in% input$cCounty)
        dat3 <- dat1 %>% #4-8-2020
          mutate(StateCounty = paste0(state,": ",county)) %>% #4-8-2020
          filter(StateCounty %in% input$cCounty) #4-8-2020
        dat <- unique(rbind(dat2,dat3))
      }
    }
    dat
  })
  
  # Get the max (latest) time entry for each county
  maxTime <- reactive({
    #6-13-2020
    if(nrow(filterCounty())>0){
      # Add a label for the max time for each county
      max_date_label <- filterCounty() %>%
        group_by(state, county) %>%
        summarise(max_time = max(time, na.rm = T))
      
      filterCounty() %>% left_join(max_date_label, by = c("state", "county"))
      #6-13-2020
    } else{
      filterCounty()
    }
  })
  
  # Get the variables that are actually being plotted
  plotVars <- reactive({
    if(input$uPlot == "cases" & input$uScale == "true"){
      plotVar = "casesPerMillion"; valueLab = "Cases Per Million"; annoTitle = paste0("Cumulative Cases Per Million by ",input$cState," County"); yAxis <- list(title = "Cumulative Cases Per Million")
    }
    if(input$uPlot == "cases" & input$uScale == "false"){
      plotVar = "cases"; valueLab = "Cases"; annoTitle = paste0("Cumulative Cases by ",input$cState," County"); yAxis <- list(title = "Cumulative Cases")
    }
    if(input$uPlot == "nCases" & input$uScale == "true"){
      plotVar = "NewCasesPerMillion"; valueLab = "New Cases Per Million"; annoTitle = paste0("New Cases Per Million by ",input$cState," County"); yAxis <- list(title = "New Cases Per Million")
    }
    if(input$uPlot == "nCases" & input$uScale == "false"){
      plotVar = "NewCases"; valueLab = "New Cases"; annoTitle = paste0("New Cases by ",input$cState," County"); yAxis <- list(title = "New Cases")
    }
    if(input$uPlot == "deaths" & input$uScale == "true"){
      plotVar = "deathsPerMillion"; valueLab = "Deaths Per Million"; annoTitle = paste0("Cumulative Deaths Per Million by ",input$cState," County"); yAxis <- list(title = "Cumulative Deaths Per Million")
    }
    if(input$uPlot == "deaths" & input$uScale == "false"){
      plotVar = "deaths"; valueLab = "Deaths"; annoTitle = paste0("Cumulative Deaths by ",input$cState," County"); yAxis <- list(title = "Cumulative Deaths")
    }
    if(input$uPlot == "nDeaths" & input$uScale == "true"){
      plotVar = "NewDeathsPerMillion"; valueLab = "New Deaths Per Million"; annoTitle = paste0("New Deaths Per Million by ",input$cState," County"); yAxis <- list(title = "New Deaths Per Million")
    }
    if(input$uPlot == "nDeaths" & input$uScale == "false"){
      plotVar = "NewDeaths"; valueLab = "New Deaths"; annoTitle = paste0("New Deaths by ",input$cState," County"); yAxis <- list(title = "New Deaths")
    }
    ##TRAVIS ZACK added for doubling time graph
    if(input$uPlot == "loessN" & input$uScale == "false"){
      plotVar = "loessN"; valueLab = "Doubling Time"; annoTitle = paste0("Case Doubling Time by ",input$cState," County"); yAxis <- list(title = "Doubling Time (days)")
    }
    ##TRAVIS ZACK added for doubling time graph
    if(input$uPlot == "loessN" & input$uScale == "true"){
      plotVar = "loessN"; valueLab = "Doubling Time"; annoTitle = paste0("Case Doubling Time by ",input$cState," County"); yAxis <- list(title = "Doubling Time (days)")
    }
    ##TRAVIS ZACK added for ICU time graph
    if(input$uPlot == "ICUbeds" & input$uScale == "false"){
      plotVar = "perc_icu_occ"; valueLab = "Estimated Percent ICU Beds Needed"; annoTitle = paste0("Estimated Percent ICU Beds Needed by ",input$cState," County"); yAxis <- list(title = "Estimated Percent ICU Beds Needed")
    }
    ##TRAVIS ZACK added for ICU time graph
    if(input$uPlot == "ICUbeds" & input$uScale == "true"){
      plotVar = "perc_icu_occ"; valueLab = "Estimated Percent ICU Beds Needed"; annoTitle = paste0("Estimated Percent ICU Beds Needed by ",input$cState," County"); yAxis <- list(title = "Estimated Percent ICU Beds Needed")
    }
    
    # format the y-axis
    yAxis[["tickformat"]] = ",d" #sets to 10,000 rather than 10k
    yAxis[["layer"]] = "below traces" #4-7-2-20
    # change to log scale (if parameter is selected)
    if(input$pScale == "log"){
      yAxis[["type"]] = "log"
      yAxis[["dtick"]] = 1 # remove minor ticks
    }
    # change to linear scale (if parameter is selected)
    if(input$pScale == "linear"){
      yAxis[["type"]] = "linear"
      yAxis[["dtick"]] = NULL # remove minor ticks
    }
    if(input$uPlot == "loessN"){
      yAxis[["autorange"]] <- "reversed"
    }
    # change to percentage if ICU is selected 8-14-2020
    if(input$uPlot == "ICUbeds"){
      yAxis[["ticksuffix"]] = '%' #sets to percentage
    }
    # Return a list based on the selected parameters
    list(plotVar=plotVar,valueLab=valueLab,annoTitle=annoTitle,yAxis=yAxis)
  })
  
  # Setup if we are using absolute or relative time
  timeUse <- reactive({
    dat <- maxTime()
    if(input$uTime == "realtiveT"){
      dat <- dat %>%
        mutate(time = time) %>%
        # Only keep points with positive time (need >= 10 cases)
        filter(time >= 0) %>%
        # Label only the latest point
        mutate(label = if_else(time == max_time, as.character(county), NA_character_))
    }
    if(input$uTime == "absoluteT"){
      #6-13-2020
      if(nrow(dat)>0){
        dat <- dat %>%
          mutate(time = date) %>%
          # Label only the latest point
          mutate(label = if_else(date == max(date,na.rm = T), as.character(county), NA_character_))
        #6-13-2020
      } else{
        dat <- dat %>%
          mutate(time = date) %>%
          mutate(label = "NA")
      }
    }
    dat$label <- as.character(dat$label)
    dat
  })
  
  # Setup x-axis labels
  xAxisLabels <- reactive({
    dat <- timeUse() #4-7-2020
    # x-axis labels
    if(input$uTime == "absoluteT"){
      xAxis <- list(title = paste0("Dates from: ",format(input$daterange1[1],"%m/%d")," - ",format(input$daterange1[2], "%m/%d")), tickformat = "%m/%d")
      xAxis[["xaxis.layer"]] = "below traces" #4-7-2020
    }
    if(input$uTime == "realtiveT"){
      xAxis <- list(title = "Days after the first 10 cases")
      xAxis[["xaxis.layer"]] = "below traces" #4-7-2020
    }
    xAxis[["autoscale"]] <- FALSE #4-7-2020
    #6-13-2020
    if(nrow(dat)>0){
      xAxis[["range"]] <- c(min(dat$time,na.rm = T), max(dat$time, na.rm = T) + (as.numeric(max(dat$time, na.rm = T)-min(dat$time,na.rm = T)))/4) #4-7-2-20
    }
    xAxis
  })
  
  # Order the counties in the lagend alphabetically (previously max value)
  orderLegend <- reactive({
    #plotVar = plotVars()[["plotVar"]]
    #maxValue <- timeUse() %>%
    #  group_by(state, county) %>%
    #  summarise(max_value = max(get(plotVar),na.rm = T)) %>%
    #  ungroup() %>%
    #  arrange(desc(max_value))
    
    # Set up the order of the counties in the legend
    #timeUse() %>%
    #  mutate(county = factor(county, levels = unique(maxValue$county)))
    timeUse() %>%
      arrange(county)
  })
  
  # If log scale add doubling lines
  doublingLines <- reactive({
    plotVar = plotVars()[["plotVar"]]
    dat <- orderLegend()
    if(input$pScale == "log"){
      # Let's not use absolute time for the lines
      # Doesn't make sense
      # if(input$uTime == "absoluteT"){
      #   startVal = 1
      #   # addVal = 0
      # }
      if(input$uTime == "realtiveT" & input$uPlot == "cases"){
        startVal = 10
        # addVal = 3
      }
      if(input$uTime == "realtiveT" & input$uPlot != "cases"){
        startVal = 1
        # addVal = 0
      }
      # Build doubling time lines
      allDates = sort(unique(dat$time[!is.infinite(dat$time)]))
      everyTwoDays = c(startVal)
      everyTwoDaysNames = c(allDates[1])
      everyFourDays = c(startVal)
      everyFourDaysNames = c(allDates[1])
      for(i in 2:as.numeric(max(dat$time[!is.infinite(dat$time)],na.rm = T)-min(dat$time[!is.infinite(dat$time)],na.rm = T))){
        if((i-1)%%2 == 0){
          # everyTwoDays = c(everyTwoDays,1)
          # everyTwoDaysNames = c(everyTwoDaysNames,1)
          toAdd = c(2*everyTwoDays[(i-1)/2])
          everyTwoDays = c(everyTwoDays,toAdd)
          # everyTwoDays = c(everyTwoDays,2^((i-1)/2+addVal))
          everyTwoDaysNames = c(everyTwoDaysNames,allDates[i])
        }
        if((i-1)%%4 == 0){
          toAdd = c(2*everyFourDays[(i-1)/4])
          everyFourDays = c(everyFourDays,toAdd)
          # everyFourDays = c(everyFourDays,2^((i-1)/4+addVal))
          everyFourDaysNames = c(everyFourDaysNames,allDates[i])
        }
      }
      everyTwoDaysNames = everyTwoDaysNames[1:length(everyTwoDays)]
      everyFourDaysNames = everyFourDaysNames[1:length(everyFourDays)]
      # Make a tibble to hold the lines
      everyTwoDays = tibble(time = everyTwoDaysNames, placeholder = everyTwoDays, county = "Doubling every 2 days", label = as.character(NA))
      everyTwoDays[which(everyTwoDays$placeholder == max(everyTwoDays$placeholder,na.rm = T)),"label"] = "Doubling every 2 days"
      everyFourDays = tibble(time = everyFourDaysNames, placeholder = everyFourDays, county = "Doubling every 4 days", label = as.character(NA))
      everyFourDays[which(everyTwoDays$placeholder == max(everyFourDays$placeholder,na.rm = T)),"label"] = "Doubling every 4 days"
      doublingLines = rbind(everyTwoDays,everyFourDays)
      colnames(doublingLines)[which(colnames(doublingLines)=="placeholder")] = plotVar
      doublingLines
    }
  })
  
  
  # Add scaling type to the titles
  scaleTitles <- reactive({
    if(input$pScale == "log"){
      scaleAnno = "Log Plot"
    }
    if(input$pScale == "linear"){
      scaleAnno = "Linear Plot"
    }
    scaleAnno
  })
  
  # Filter zero entries if we are doing a log plot
  filterLogZeros <- reactive({
    plotVar = plotVars()[["plotVar"]]
    dat <- orderLegend()
    if(input$pScale == "log"){
      dat <- dat %>%
        filter(get(plotVar) > 0)
    }
    if(input$pScale == "linear"){
      dat <- dat
    }
    dat
  })
  
  ######################################################################
  #  US map plot filter functions
  ######################################################################
  
  # Filter the date range for the data to show on the US map
  filterUSdates <- reactive({
    # Pull in the state data
    req(stateDat)
    
    # Get the appropriate date range
    dat1 <- subset(stateDat, as.character(date) == as.character(as.Date(input$daterange1[1])) )
    # Add exception if no state is selected
    validate(
      need(nrow(dat1) >0, 'Please choose a date range for the state which has available data.')
    )
    dat2 <- subset(stateDat, as.character(date) == as.character(as.Date(input$daterange1[2])) )
    # Find the states for that don't have data in the 1st timepoint but do have data in the 2nd timepoint
    toAdd = dat2[which(!dat2$state %in% dat1$state),]
    if(nrow(toAdd)>0){
      toAdd[,c("cases","deaths")] = 0
      dat1 = rbind(dat1,toAdd)
    }
    dat2 <- dat2 %>% 
      arrange(state)
    dat1 <- dat1 %>% 
      arrange(state)
    
    # Get the difference between the two date ranges (e.g. we want the number of cases or deaths for that date range)
    dat2$cases = dat2$cases - dat1$cases
    dat2$deaths = dat2$deaths - dat1$deaths
    
    dat2
  })
  
  # Get the variables that are actually being plotted
  plotVarsUSmap <- reactive({
    if( input$uEvents == "default" ){
      if(input$uPlot == "cases" & input$uScale == "true" ){
        plotVar = "casesPerMillion"; annoTitle = paste0("Cumulative Cases Per Million by State from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of cases - orange; low number of cases - white)" #4-7-2-20
      }
      if(input$uPlot == "cases" & input$uScale == "false"){
        plotVar = "cases"; annoTitle = paste0("Cumulative Cases by State from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of cases - orange; low number of cases - white)" #4-7-2-20
      }
      if(input$uPlot == "nCases" & input$uScale == "true"){
        plotVar = "NewCasesPerMillion"; annoTitle = paste0("New Cases Per Million by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of cases - orange; low number of cases - white)" #4-7-2-20
      }
      if(input$uPlot == "nCases" & input$uScale == "false" ){
        plotVar = "NewCases"; annoTitle = paste0("New Cases by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of cases - orange; low number of cases - white)" #4-7-2-20
      }
      if(input$uPlot == "deaths" & input$uScale == "true"){
        plotVar = "deathsPerMillion"; annoTitle = paste0("Cumulative Deaths Per Million by State from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of deaths - orange; low number of deaths - white)" #4-7-2-20
      }
      if(input$uPlot == "deaths" & input$uScale == "false" ){
        plotVar = "deaths"; annoTitle = paste0("Cumulative Deaths by State from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of deaths - orange; low number of deaths - white)" #4-7-2-20
      }
      if(input$uPlot == "nDeaths" & input$uScale == "true"){
        plotVar = "NewDeathsPerMillion"; annoTitle = paste0("New Deaths Per Million by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of deaths - orange; low number of deaths - white)" #4-7-2-20
      }
      if(input$uPlot == "nDeaths" & input$uScale == "false" ){
        plotVar = "NewDeaths"; annoTitle = paste0("New Deaths by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high number of deaths - orange; low number of deaths - white)" #4-7-2-20
      }
      ##TRAVIS ZACK added for doubling time graph
      if(input$uPlot == "loessN" & input$uScale == "false" ){
        plotVar = "loessN"; annoTitle = paste0("Case Doubling Time by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(low doubling time - orange; high doubling time - white)" #4-7-2-20
      }
      ##TRAVIS ZACK added for doubling time graph
      if(input$uPlot == "loessN" & input$uScale == "true"){
        plotVar = "loessN"; annoTitle = paste0("Case Doubling Time by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(low doubling time - orange; high doubling time - white)" #4-7-2-20
      }
      ##TRAVIS ZACK added for ICU time graph
      if(input$uPlot == "ICUbeds" & input$uScale == "false"){
        plotVar = "perc_icu_occ"; annoTitle = paste0("Estimated Percent ICU Beds Needed by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high fraction of estimated ICU beds needed - orange; low fraction - white)" #4-7-2-20
      }
      ##TRAVIS ZACK added for ICU time graph
      if(input$uPlot == "ICUbeds" & input$uScale == "true" ){
        plotVar = "perc_icu_occ"; annoTitle = paste0("Estimated Percent ICU Beds Needed by State on: ",format(input$daterange1[2], "%m/%d/%Y")) #10-8-2020
        annoSubtitle = "(high fraction of estimated ICU beds needed - orange; low fraction - white)" #4-7-2-20
      }
    }
    
    # DVA added
    # Map the state mandated events
    #if(input$uEvents != "default"){ #4-7-2-20
    # if(input$uEvents == "sd"){
    #   plotVar = "SD"
    #   annoTitle = paste0("State Mandated Social Distancing\nFirst Date: ",format(min(as.Date(stateDat$SD),na.rm = T), "%m/%d"))
    # }
    if(input$uEvents == "soe"){
      plotVar = "SE"
      annoTitle = paste0("State Declared State of Emergency\nFirst Date: ",format(min(as.Date(stateDat$SE),na.rm = T), "%m/%d/%Y")) #10-8-2020
      annoSubtitle = "(later date - orange; earlier date - white)" #4-7-2-20
    }
    if(input$uEvents == "cs"){
      plotVar = "CS"
      annoTitle = paste0("State Mandated Closure of Public Schools\nFirst Date: ",format(min(as.Date(stateDat$CS),na.rm = T), "%m/%d/%Y")) #10-8-2020
      annoSubtitle = "(later date - orange; earlier date - white)" #4-7-2-20
    }
    if(input$uEvents == "sip"){
      plotVar = "SIP"
      annoTitle = paste0("State Mandated Shelter in Place\nFirst Date: ",format(min(as.Date(stateDat$SIP),na.rm = T), "%m/%d/%Y")) #10-8-2020
      annoSubtitle = "(later date - orange; earlier date - white)" #4-7-2-20
    }
    if(input$uEvents == "rb"){
      plotVar = "RB"
      annoTitle = paste0("State Mandated Closure of Restaurants/Bars\nFirst Date: ",format(min(as.Date(stateDat$RB),na.rm = T), "%m/%d/%Y")) #10-8-2020
      annoSubtitle = "(later date - orange; earlier date - white)" #4-7-2-20
    }
    #}
    
    # Return a list based on the selected parameters
    list(plotVar=plotVar,annoTitle=annoTitle,annoSubtitle=annoSubtitle) # TEMPORARILY removed- annoSubtitle) #4-7-2-20
  })
  
  #phdva
  
  # Convert values to percentiles
  usMapPercentile <- reactive({
    plotVar = plotVarsUSmap()[["plotVar"]]
    dat <- filterUSdates()
    # DVA added -- don't do percentage if it is dates
    if(input$uEvents == "default"){
      # dat[,plotVar] = perc.rank(pull(dat[,plotVar]))
      # dat[,plotVar] = pull(dat[,plotVar])*100
      dat$value = perc.rank(dat[[plotVar]])
      # dat$value = perc.rank(pull(dat[,plotVar]))
      dat$value = dat$value*100
      # If dates, get time since first occurance
      # Get percentile from the earliest adoption
    }
    if(input$uEvents != "default" & input$uEvents %in% c("soe","cs","sip","rb")){ #4-7-2-20
      # if(input$uEvents != "default" & input$uEvents %in% c("sd","soe","cs","sip","rb")){
      dateDiff = as.numeric(as.Date(dat[[plotVar]]) - min(as.Date(dat[[plotVar]]),na.rm = T))
      naIx <- which(is.na(dateDiff))
      if(length(naIx) > 0){
        dateDiff[-naIx] <- perc.rank(dateDiff[-naIx])
      } else{
        dateDiff <- perc.rank(dateDiff)
      }
      dat$value = dateDiff*100
    }
    dat
  })
  
  # # Convert values to percentiles
  # usMapPercentile <- reactive({
  #   plotVar = plotVars()[["plotVar"]]
  #   dat <- filterUSdates()
  #   # dat[,plotVar] = perc.rank(pull(dat[,plotVar]))
  #   # dat[,plotVar] = pull(dat[,plotVar])*100
  #   dat$value = perc.rank(pull(dat[,plotVar]))
  #   dat$value = dat$value*100
  #   dat
  # })
  
  # Get the state boundaries
  buildStates <- reactive({
    req(states_sf)
    plotVar = plotVarsUSmap()[["plotVar"]]
    dat <- usMapPercentile()
    dat <- states_sf %>%
      left_join(dat,by = c("state_name" = "state")) %>%
      # transform the coordinates of the map (by default it is very squished)
      st_transform(crs = paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
    # add a color mapping to allow us to recolor the maps without have to redraw them
    # colorCuts <- cut(dat[[plotVar]], breaks = seq(min(dat[[plotVar]],na.rm = T), max(dat[[plotVar]],na.rm = T), len = 100), include.lowest = TRUE)
    if(length(dat$value) == 1){
      dat$colorMap = "#e6550d"
    } else{
      if(length(which(is.infinite(dat$value)))>0){  #6-13-2020
        colorCuts <- cut(dat$value, breaks = seq(min(dat$value[-which(is.infinite(dat$value))],na.rm = T), max(dat$value[-which(is.infinite(dat$value))],na.rm = T), len = 100), include.lowest = TRUE) #6-13-2020
      } else{
        colorCuts <- cut(dat$value, breaks = seq(min(dat$value,na.rm = T), max(dat$value,na.rm = T), len = 100), include.lowest = TRUE) #6-13-2020
      }
    }
    
    #4-7-2-20
    if(plotVar != "loessN"){
      dat$colorMap <- colorRampPalette(colors = c("#ffffff", "#ffbe87", "#e6550d"))(99)[colorCuts]
    }
    
    #4-7-2-20
    # If doubling time, reverse the color order
    if(plotVar == "loessN"){
      dat$colorMap <- colorRampPalette(colors = c("#e6550d", "#ffbe87", "#ffffff"))(99)[colorCuts]
    }
    
    # Different N/A colors for social policies
    if(plotVar %in% c("SE","CS","SIP","RB")){
      naIx <- which(is.na(dat$colorMap))
      if(length(naIx) > 0){
        dat$colorMap[which(is.na(dat$colorMap))] <- "#e6550d"
      }
      # dat$colorMap <- colorRampPalette(colors = c("#e6550d", "#ffbe87", "#ffffff"))(99)[colorCuts]
    } else{
      
      # add N/A color to the colormap
      naIx <- which(is.na(dat$colorMap))
      if(length(naIx) > 0){
        dat$colorMap[which(is.na(dat$colorMap))] <- "#bfbfbf"
      }
    }
    
    
    dat
  })
  
  ######################################################################
  #  State map plot filter functions
  ######################################################################
  
  #ph1
  
  # Get the variables that are actually being plotted
  plotVarsStateMap <- reactive({
    
    if(input$uPlot == "cases" & input$uScale == "true"){
      plotVar = "casesPerMillion"
      annoTitle = paste0("Cumulative Cases Per Million by ",input$cState," County from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    if(input$uPlot == "cases" & input$uScale == "false"){
      plotVar = "cases"
      annoTitle = paste0("Cumulative Cases by ",input$cState," County from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    if(input$uPlot == "nCases" & input$uScale == "true"){
      plotVar = "NewCasesPerMillion"
      annoTitle = paste0("New Cases Per Million by ",input$cState," County on: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    if(input$uPlot == "nCases" & input$uScale == "false"){
      plotVar = "NewCases"
      annoTitle = paste0("New Cases by ",input$cState," County on: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    if(input$uPlot == "deaths" & input$uScale == "true"){
      plotVar = "deathsPerMillion"
      annoTitle = paste0("Cumulative Deaths Per Million by ",input$cState," County from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    if(input$uPlot == "deaths" & input$uScale == "false"){
      plotVar = "deaths"
      annoTitle = paste0("Cumulative Deaths by ",input$cState," County from: ",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    if(input$uPlot == "nDeaths" & input$uScale == "true"){
      plotVar = "NewDeathsPerMillion"
      annoTitle = paste0("New Deaths Per Million by ",input$cState," County on: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    if(input$uPlot == "nDeaths" & input$uScale == "false"){
      plotVar = "NewDeaths"
      annoTitle = paste0("New Deaths by ",input$cState," County on: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    ##Doug added for doubling time graph (based on Travis's code)
    if(input$uPlot == "loessN" & input$uScale == "false"){
      plotVar = "loessN"
      annoTitle = paste0("Case Doubling Time by ",input$cState," County: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    ##Doug added for doubling time graph (based on Travis's code)
    if(input$uPlot == "loessN" & input$uScale == "true"){
      plotVar = "loessN"
      annoTitle = paste0("Case Doubling Time by ",input$cState," County: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    ##TRAVIS ZACK added for ICU time graph
    if(input$uPlot == "ICUbeds" & input$uScale == "false"){
      plotVar = "perc_icu_occ"
      annoTitle = paste0("Estimated Percent ICU Beds Needed by ",input$cState," County: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    ##TRAVIS ZACK added for ICU time graph
    if(input$uPlot == "ICUbeds" & input$uScale == "true"){
      plotVar = "perc_icu_occ"
      annoTitle = paste0("Estimated Percent ICU Beds Needed by ",input$cState," County: ",format(input$daterange1[2], "%m/%d/%Y") ) #10-8-2020
    }
    
    # Return a list based on the selected parameters
    list(plotVar=plotVar,annoTitle=annoTitle)
  })
  
  # Filter the date range for the data to show in the state map
  filterStateDates <- reactive({
    # Pull in the county data
    req(countyDat)
    dat <- countyDat
    
    # Join the state spatial data with the covid-19 data
    # Get the appropriate date range
    dat1 = dat[which(dat$state == input$cState & dat$date == input$daterange1[1]),]
    
    # # Add exception if no state is selected
    # validate(
    #   need(nrow(dat1) >0, 'Please choose a date range for the state which has available data.')
    # )
    
    dat = dat[which(dat$state == input$cState & dat$date == input$daterange1[2]),]
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
    
    dat
  })
  
  # Convert values to percentiles (within the state)
  stateMapPercentile <- reactive({
    plotVar = plotVarsStateMap()[["plotVar"]]
    dat <- filterStateDates()
    
    # Scale to percentile
    # DA added
    valUse <- pull(dat[,plotVar])
    naIx <- which(is.na(valUse))
    if(length(naIx) > 0){
      valUse[-naIx] <- perc.rank(valUse[-naIx])
    } else{
      valUse <- perc.rank(valUse)
    }
    dat$value = valUse
    dat$value = dat$value*100
    # #Old
    # # Percentile within the state
    # dat$value = perc.rank(pull(dat[,plotVar]))
    # dat$value = dat$value*100
    dat
  })
  
  # Get the county boundaries
  buildCounties <- reactive({
    # require the county boundaries
    req(counties_sf)
    plotVar = plotVarsStateMap()[["plotVar"]]
    dat <- stateMapPercentile()
    counties_sf_state <- subset(counties_sf, state_name == input$cState)
    dat <- left_join(counties_sf_state, dat, by = c("county_fips" = "fips"))
    
    # Rename columns for plotting
    dat <- dat %>%
      mutate(State = state_name) %>%
      mutate(County = county_name) %>%
      # Capitalize the county and sate names
      mutate(County = str_to_title(County)) %>%
      mutate(State = str_to_title(State))
    
    dat
  })
  
  # Do the state projection (need to center and rotate)
  stateProjection <- reactive({
    
    dat <- buildCounties()
    # Can't do projection for alaska or hawaii; nebraska looks better without (it gets smushed)
    if(!input$cState %in% c("Alaska","Hawaii")){
      
      if(input$cState %in% c("Nebraska","Indiana", "South Carolina","Delaware","District of Columbia")){
        
        statesLimits <- map_data("state")
        statesLimits = statesLimits[which(statesLimits$region == tolower(input$cState)),]
        minLat = min(statesLimits$lat); maxLat = max(statesLimits$lat); minLon = min(statesLimits$long); maxLon = max(statesLimits$long)
        midLat = (maxLat + minLat)/2; midLon = (maxLon + minLon)/2
        
        dat <- dat %>%
          st_transform(crs = paste0("+proj=aea +lat_1=",minLat," +lat_2=",maxLat," +lat_0=",midLat," +lon_0=",midLon," +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
        
      } else{
        if(input$cState %in% c("Massachusetts","Washington","North Dakota","Ohio","New Mexico","Minnesota","Mississippi","Florida","Alabama")){
          # Need to fix some states (they get squished)
          if(input$cState %in% c("Massachusetts")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "island")
          }
          if(input$cState %in% c("Washington","North Dakota","Ohio")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "south")
          }
          if(input$cState %in% c("New Mexico")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "east")
          }
          if(input$cState %in% c("Minnesota")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "north")
          }
          if(input$cState %in% c("Mississippi","Florida")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "west")
          }
          if(input$cState %in% c("Alabama")){
            planeUse = "+proj=aea +lat_1=30.24071 +lat_2=35.01345 +lat_0=32.318231 +lon_0=-86.68851 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
          }
        }
        else{
          planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"))
        }
        dat <- dat %>%
          st_transform(crs = planeUse)
      }
    }
    # add a color mapping to allow us to recolor the maps without have to redraw them
    
    #phDVA
    # This does not display the error message.....
    # Add exception if we don't have enough counties to color
    validate(
      need(length(which(!is.na(dat$value))) > 1, 'Please choose options that have more than 1 county with data.')
    )
    if(length(dat$value) == 1){
      dat$colorMap = "#e6550d"
    } else{
      #asdf2
      if(length(unique(dat$value[!is.na(dat$value)]))<3){
        colorMap <- rep(NA,length(dat$value))
        colorMap[which(!is.na(dat$value))] <- rep("#ffffff",length(dat$value[!is.na(dat$value)]))
        dat$colorMap <- colorMap
      } else{
        colorCuts <- cut(dat$value, breaks = seq(min(dat$value,na.rm = T), max(dat$value,na.rm = T), len = 100), include.lowest = TRUE)
        dat$colorMap <- colorRampPalette(colors = c("#ffffff", "#ffbe87", "#e6550d"))(99)[colorCuts]
      }
    }
    
    # DA added
    # naIx <- which(is.na(pull(dat[,defaultValPlot])))
    naIx <- which(is.na(dat$colorMap))
    if(length(naIx) > 0){
      dat$colorMap[naIx] <- "#BFBFBF"
    }
    dat
  })
  
  
  ######################################################################
  #  Main Plot
  ######################################################################
  output$stateLinePlot <- renderPlotly({
    
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    
    # When we start up, the county is null bc of how we are assigning things -- don't draw plot until we get it
    validate(need(!is.null(input$cCounty), message=FALSE))
    
    # print(input$cState)
    # print(input$cCounty)
    # print(input$mCases)
    # print(input$daterange1)
    # print(input$uPlot)
    # print(input$uScale)
    # print(input$pScale)
    # print(input$uTime)
    
    # trigger changes here:
    # input$cState; plotVars(); filterLogZeros()
    #; scaleTitles(); filterLogZeros(); doublingLines()
    
    ##TRAVIS ZACK### THese lines just create the text to print the fastest doubling time
    cur_dbl_df <- dbl_df2[which(dbl_df2['state']==input$cState),]
    fastest_dbl <- cur_dbl_df[which(cur_dbl_df$cur_double==min(cur_dbl_df$cur_double,na.rm=TRUE)),]
    #tl_str <- paste('Fastest doubling time in state is\n ',fastest_dbl$county,': ',as.character(round(fastest_dbl$cur_double,digits=3)), 'days')
    # print(tl_str)
    
    # Create name of state over graph
    output$state_title = renderText({  input$cState  })
    # Creat little message under graph that (usually) talks about doubling time
    output$doubling_time = renderText({ 
      if( input$uPlot == "ICUbeds" ){
        paste("Estimations assume a 4.43% hospitalization rate, 20.1% ICU rate") #12-15-2020
      }else{
        paste('Fastest doubling time in state is ',fastest_dbl$county,': ',as.character(round(fastest_dbl$cur_double)), 'days')         
      }
    })
    # Ceate main plot title
    output$main_plot_title = renderText({  paste0(plotVars()[["annoTitle"]]," - ",scaleTitles()) })
    
    # Actually plot everything
    p1 <- plot_ly(data = filterLogZeros(),
                  x = ~time, 
                  y = ~get(plotVars()[["plotVar"]]),
                  # sort = FALSE, # makes alphabetical?
                  # colors = iwanthue(n=length(unique(filterLogZeros()$county))),
                  split = ~county,
                  key = ~county,
                  type = 'scatter',
                  mode = 'lines+text',
                  text = ~label,
                  textposition = "middle right",
                  cliponaxis = FALSE, #4-7-2-20
                  height = 515, #4-8-2020
                  hoverinfo = 'text',
                  hovertext = ~paste0('</br>State: ', str_to_title(state),
                                      '</br>County: ', str_to_title(county),
                                      '</br>Date: ', time,
                                      '</br>',plotVars()[["valueLab"]],': ', format(round(get(plotVars()[["plotVar"]]), digits = 1), big.mark=",",scientific=FALSE)
                  )
    )
    
    # SPEED THIS UP -- MAKE THIS A SINGLE SEGMENT FROM A SINGLE DATAFRAME (MAKE THIS NOT DEPEND ON THE PREVIOUS DATA) -- I THINK THIS IS SLOWING IT DOWN
    
    # Add in state policies
    if(input$dArrows == "true"){
      if(length(pull(filterLogZeros()[,plotVars()[["plotVar"]]])) > 0){
        # If doubling time, need to flip the labels
        if(input$uPlot == "loessN"){
          minVal <- max(pull(filterLogZeros()[,plotVars()[["plotVar"]]]), na.rm = T)
          maxVal <- min(pull(filterLogZeros()[,plotVars()[["plotVar"]]]), na.rm = T)
          totVal <- maxVal - minVal
        } else{
          maxVal <- max(pull(filterLogZeros()[,plotVars()[["plotVar"]]]), na.rm = T)
          minVal <- min(pull(filterLogZeros()[,plotVars()[["plotVar"]]]), na.rm = T)
          totVal <- maxVal - minVal
        }
        
        csDate <- unique(pull(stateDat[which(stateDat$state == input$cState),"CS"]))
        rbDate <- unique(pull(stateDat[which(stateDat$state == input$cState),"RB"]))
        seDate <- unique(pull(stateDat[which(stateDat$state == input$cState),"SE"]))
        sipDate <- unique(pull(stateDat[which(stateDat$state == input$cState),"SIP"]))
        
        if(csDate == rbDate & !is.na(csDate) & !is.na(rbDate)){
          p1 <- p1 %>%
            add_segments(inherit = FALSE, x = csDate, xend = csDate,
                         y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                         hovertext = paste0('</br>State: ', input$cState,
                                            "</br>School Closure: ",format(csDate, "%m/%d/%Y"), #10-8-2020
                                            "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"))) %>% #10-8-2020
            add_text(inherit = FALSE, x = csDate, y = maxVal, text = "CS,R/B", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                     hovertext = paste0('</br>State: ', input$cState,
                                        "</br>School Closure: ",format(csDate, "%m/%d/%Y"), #10-8-2020
                                        "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"))) #10-8-2020
        } else{
          if(csDate == seDate & !is.na(csDate) & !is.na(seDate)){
            p1 <- p1 %>%
              add_segments(inherit = FALSE, x = csDate, xend = csDate,
                           y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                           hovertext = paste0('</br>State: ', input$cState,
                                              "</br>School Closure: ",format(csDate, "%m/%d/%Y"), #10-8-2020
                                              "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"))) %>% #10-8-2020
              add_text(inherit = FALSE, x = csDate, y = maxVal, text = "CS,SoE", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                       hovertext = paste0('</br>State: ', input$cState,
                                          "</br>School Closure: ",format(csDate, "%m/%d/%Y"), #10-8-2020
                                          "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"))) #10-8-2020
          } else{
            if(csDate == sipDate & !is.na(csDate) & !is.na(sipDate)){
              p1 <- p1 %>%
                add_segments(inherit = FALSE, x = csDate, xend = csDate,
                             y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                             hovertext = paste0('</br>State: ', input$cState,
                                                "</br>School Closure: ",format(csDate, "%m/%d/%Y"), #10-8-2020
                                                "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) %>% #10-8-2020
                add_text(inherit = FALSE, x = csDate, y = maxVal, text = "CS,SiP", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                         hovertext = paste0('</br>State: ', input$cState,
                                            "</br>School Closure: ",format(csDate, "%m/%d/%Y"), #10-8-2020
                                            "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) #10-8-2020
            } else{
              if(!is.na(csDate)){
                p1 <- p1 %>%
                  add_segments(inherit = FALSE, x = csDate, xend = csDate,
                               y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                               hovertext = paste0('</br>State: ', input$cState,
                                                  "</br>School Closure: ",format(csDate, "%m/%d/%Y"))) %>% #10-8-2020
                  add_text(inherit = FALSE, x = csDate, y = maxVal, text = "CS", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                           hovertext = paste0('</br>State: ', input$cState,
                                              "</br>School Closure: ",format(csDate, "%m/%d/%Y"))) #10-8-2020
              }
            }
          }
        }
        
        if(rbDate == seDate & !is.na(rbDate) & !is.na(seDate)){
          p1 <- p1 %>%
            add_segments(inherit = FALSE, x = rbDate, xend = rbDate,
                         y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                         hovertext = paste0('</br>State: ', input$cState,
                                            "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"), #10-8-2020
                                            "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"))) %>% #10-8-2020
            add_text(inherit = FALSE, x = rbDate, y = maxVal, text = "R/B,SoE", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                     hovertext = paste0('</br>State: ', input$cState,
                                        "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"), #10-8-2020
                                        "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"))) #10-8-2020
        } else{
          if(rbDate == sipDate & !is.na(rbDate) & !is.na(sipDate)){
            p1 <- p1 %>%
              add_segments(inherit = FALSE, x = rbDate, xend = rbDate,
                           y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                           hovertext = paste0('</br>State: ', input$cState,
                                              "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"), #10-8-2020
                                              "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) %>% #10-8-2020
              add_text(inherit = FALSE, x = rbDate, y = maxVal, text = "R/B,SiP", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                       hovertext = paste0('</br>State: ', input$cState,
                                          "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"), #10-8-2020
                                          "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) #10-8-2020
          } else{
            if(rbDate != csDate & !is.na(rbDate)){
              p1 <- p1 %>%
                add_segments(inherit = FALSE, x = rbDate, xend = rbDate,
                             y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                             hovertext = paste0('</br>State: ', input$cState,
                                                "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"))) %>% #10-8-2020
                add_text(inherit = FALSE, x = rbDate, y = maxVal, text = "R/B", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                         hovertext = paste0('</br>State: ', input$cState,
                                            "</br>Restuarant/Bar Closure: ",format(rbDate, "%m/%d/%Y"))) #10-8-2020
            }
          }
        } 
        
        if(seDate == sipDate  & !is.na(seDate) & !is.na(sipDate)){
          p1 <- p1 %>%
            add_segments(inherit = FALSE, x = seDate, xend = seDate,
                         y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                         hovertext = paste0('</br>State: ', input$cState,
                                            "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"), #10-8-2020
                                            "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) %>% #10-8-2020
            add_text(inherit = FALSE, x = seDate, y = maxVal, text = "SoE,SiP", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                     hovertext = paste0('</br>State: ', input$cState,
                                        "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"), #10-8-2020
                                        "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) #10-8-2020
        } else{
          if(seDate != csDate & seDate != rbDate & !is.na(seDate)){
            p1 <- p1 %>%
              add_segments(inherit = FALSE, x = seDate, xend = seDate,
                           y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                           hovertext = paste0('</br>State: ', input$cState,
                                              "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"))) %>% #10-8-2020
              add_text(inherit = FALSE, x = seDate, y = maxVal, text = "SoE", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                       hovertext = paste0('</br>State: ', input$cState,
                                          "</br>State of Emergency: ",format(seDate, "%m/%d/%Y"))) #10-8-2020
          }
        }
        
        if(sipDate != csDate & sipDate != rbDate & sipDate != seDate & !is.na(sipDate)){
          p1 <- p1 %>%
            add_segments(inherit = FALSE, x = sipDate, xend = sipDate,
                         y = minVal, yend = maxVal, showlegend=FALSE, line=list(color = 'rgba(0,0,0,1)', dash = 'dash'), hoverinfo = 'text',
                         hovertext = paste0('</br>State: ', input$cState,
                                            "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) %>% #10-8-2020
            add_text(inherit = FALSE, x = unique(pull(stateDat[which(stateDat$state == input$cState),"SIP"])), y = maxVal, text = "SiP", textposition = "top middle", showlegend=FALSE, hoverinfo = 'text',
                     hovertext = paste0('</br>State: ', input$cState,
                                        "</br>Shelter in Place: ",format(sipDate, "%m/%d/%Y"))) #10-8-2020
        }
        
      }
    }
    
    # Add doubling lines if we are in log scale
    if(input$pScale == "log" & input$uTime == "realtiveT" & input$uPlot %in% c("cases","deaths","nCases","nDeaths")){
      doublingLines <- doublingLines()
      doublingLines$label = as.character(doublingLines$label)
      p1 <- p1  %>%
        add_trace(data = doublingLines,
                  # add_trace(data = doublingLines()[which(doublingLines()$time >= min(filterLogZeros()$time) & doublingLines()$time <= max(filterLogZeros()$time)),],
                  x = ~time,
                  y = ~get(plotVars()[["plotVar"]]),
                  # colors = iwanthue(n=length(unique(filterLogZeros()$county))),
                  split = ~county,
                  key = ~county,
                  type = 'scatter',
                  mode = 'lines+text',
                  text = ~label,
                  textposition = "middle right",
                  cliponaxis = FALSE, #4-7-2-20
                  line = list(color = '#404040', width = 3, dash = 'dash'),
                  hoverinfo = 'text',
                  inherit = F,
                  hovertext = ~paste0('</br>',county
                  )
        )
    }
    
    # Print the plot
    p1 %>% layout(yaxis = plotVars()[["yAxis"]],
                  xaxis = list(title = " . "), #xAxisLabels(),
                  #xAxis <- list(title = ""),
                  legend = list(x = 0, #4-8-2020
                                xanchor = "left", #4-8-2020
                                y = -0.1, #7-8-2020
                                yanchor = "top", #4-8-2020
                                orientation = "h",
                                font = list(size = 10)), #4-8-2020
                  # autosize = TRUE, #4-8-2020
                  #legend = list(x = 0.1, y = 0.9),
                  # to add the doubling time from Travis
                  annotations = list(text = "",  x = 0.15, xref = 'paper', y = 0.8, yref = 'paper', showarrow=FALSE) )
    #)
    
    #partial_bundle(local = FALSE) %>%
    #plotly_mod_dep() #%>%
    #toWebGL()
    
  })
  
  ######################################################################
  #  US Map -- generate a map based on the default parameters
  ######################################################################
  
  # Generate the initial output based on default parameters
  output$usaPlot <- renderPlotly({
    
    # Handle when max date value is selected: 6-23-2020
    validate(
      need(input$daterange1[[2]] != max(countyDat$date), 'end date selected is beyond available data.')
    )
    
    # Title for the US Map plot
    output$us_map_title = renderText({  plotVarsUSmap()[["annoTitle"]] })
    # Add a subtitle to help interpret the legends
    output$percentile_info = renderText({ plotVarsUSmap()[["annoSubtitle"]] }) #4-7-2-20
    output$percentile_info2 = renderText({ plotVarsUSmap()[["annoSubtitle"]] }) #4-7-2-20
    
    isolate(p2 <- plot_ly(colors = colorPallette, 
                          source = "usaPlot",
                          color = ~value ) %>%
              
              
              # If we have N/As -- make them grey
              add_sf(data = usmapDat2[which(is.na(usmapDat2$value)),],
                     inherit = F,
                     type = "scatter", #6-13-2020
                     split = ~state_name,
                     key = ~state_name,
                     color = I("gray75"),
                     alpha = 1,
                     stroke = I("gray50"),
                     span = I(1),
                     hoverinfo = "text",
                     hoveron = "fills",
                     text = ~paste0('</br>State: ', str_to_title(state_name),
                                    "</br>Doubling time (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(loessN, digits = 1),big.mark=",",scientific=FALSE), " days", #10-8-2020
                                    '</br>Total ICU Beds: ', format(round(ICUbeds, digits = 1),big.mark=",",scientific=FALSE),
                                    '</br>Estimated ICU Beds Needed (',format(defaultDate2, "%m/%d/%Y"),"): ", format(round(icu_bed_occ, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                    '</br>Fraction ICU Beds Needed (',format(defaultDate2, "%m/%d/%Y"),"): ", format(round(perc_icu_occ, digits = 1),big.mark=",",scientific=FALSE), "%", #10-8-2020
                                    '</br>Cases (', format(defaultDate1, "%m/%d/%Y"), ' - ', format(defaultDate2, "%m/%d/%Y"), '): ', format(cases,big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>Deaths (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(deaths,big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>Cases Per Million (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(casesPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>Deaths Per Million (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(deathsPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>New Cases (",format(defaultDate2, "%m/%d/%Y"),"): ", format(NewCases,big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>New Deaths (",format(defaultDate2, "%m/%d/%Y"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>New Cases Per Million (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(NewCasesPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>New Deaths Per Million (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(NewDeathsPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                    "</br>State of Emergency: ",format(as.Date(SE), "%m/%d/%Y"), #10-8-2020
                                    "</br>School Closure: ",format(as.Date(CS), "%m/%d/%Y"), #10-8-2020
                                    "</br>Shelter in Place: ",format(as.Date(SIP), "%m/%d/%Y"), #10-8-2020
                                    "</br>Bar/Restuarant Closure: ",format(as.Date(RB), "%m/%d/%Y")#, #4-7-2-20 #10-8-2020
                                    # "</br>Social Distancing: ",format(as.Date(SD), "%m/%d") #4-7-2-20
                     )
              )
    )
    # The actual data we are plotting
    isolate(p2 <- add_sf(p2, data = usmapDat2,
                         # isolate(p2 <- add_sf(p2, data = dat2[which(!is.na(dat2$value)),],
                         type = "scatter", #6-13-2020
                         split = ~state_name,
                         key = ~state_name,
                         alpha = 1,
                         stroke = I("gray50"),
                         span = I(1),
                         hoverinfo = "text",
                         hoveron = "fills",
                         text = ~paste0('</br>State: ', str_to_title(state_name),
                                        "</br>Doubling time (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(loessN, digits = 1),big.mark=",",scientific=FALSE), " days", #10-8-2020
                                        '</br>Total ICU Beds: ', format(round(ICUbeds, digits = 1),big.mark=",",scientific=FALSE),
                                        '</br>Estimated ICU Beds Needed (',format(defaultDate2, "%m/%d/%Y"),"): ", format(round(icu_bed_occ, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        '</br>Fraction ICU Beds Needed (',format(defaultDate2, "%m/%d/%Y"),"): ", format(round(perc_icu_occ, digits = 1),big.mark=",",scientific=FALSE), "%", #10-8-2020
                                        '</br>Cases (', format(defaultDate1, "%m/%d/%Y"), ' - ', format(defaultDate2, "%m/%d/%Y"), '): ', format(cases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(deaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Cases Per Million (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(casesPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths Per Million (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(deathsPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases (",format(defaultDate2, "%m/%d/%Y"),"): ", format(NewCases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths (",format(defaultDate2, "%m/%d/%Y"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases Per Million (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(NewCasesPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths Per Million (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(NewDeathsPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>State of Emergency: ",format(as.Date(SE), "%m/%d/%Y"), #10-8-2020
                                        "</br>School Closure: ",format(as.Date(CS), "%m/%d/%Y"), #10-8-2020
                                        "</br>Shelter in Place: ",format(as.Date(SIP), "%m/%d/%Y"), #10-8-2020
                                        "</br>Bar/Restuarant Closure: ",format(as.Date(RB), "%m/%d/%Y")#, #4-7-2-20 #10-8-2020
                                        #"</br>Social Distancing: ",format(as.Date(SD), "%m/%d") #4-7-2-20
                         )
    )
    )
    # State highlight
    isolate(p2 <- add_sf(p2,
                         inherit = F,
                         type = "scatter", #6-13-2020
                         data = usmapDat2[which(usmapDat2$state_name == defaultState),],
                         split = ~state_name,
                         key = ~state_name,
                         alpha = 0,
                         stroke = I("#00FFFF"), #highlight cyan
                         span = I(2),
                         hoverinfo = "text",
                         hoveron = "fills",
                         text = ~paste0('</br>State: ', str_to_title(state_name),
                                        "</br>Doubling time (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(loessN, digits = 1),big.mark=",",scientific=FALSE), " days", #10-8-2020
                                        '</br>Total ICU Beds: ', format(round(ICUbeds, digits = 1),big.mark=",",scientific=FALSE),
                                        '</br>Estimated ICU Beds Needed (',format(defaultDate2, "%m/%d/%Y"),"): ", format(round(icu_bed_occ, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        '</br>Fraction ICU Beds Needed (',format(defaultDate2, "%m/%d/%Y"),"): ", format(round(perc_icu_occ, digits = 1),big.mark=",",scientific=FALSE), "%", #10-8-2020
                                        '</br>Cases (', format(defaultDate1, "%m/%d/%Y"), ' - ', format(defaultDate2, "%m/%d/%Y"), '): ', format(cases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(deaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Cases Per Million (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(casesPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths Per Million (",format(defaultDate1, "%m/%d/%Y")," - ",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(deathsPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases (",format(defaultDate2, "%m/%d/%Y"),"): ", format(NewCases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths (",format(defaultDate2, "%m/%d/%Y"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases Per Million (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(NewCasesPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths Per Million (",format(defaultDate2, "%m/%d/%Y"),"): ", format(round(NewDeathsPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>State of Emergency: ",format(as.Date(SE), "%m/%d/%Y"), #10-8-2020
                                        "</br>School Closure: ",format(as.Date(CS), "%m/%d/%Y"), #10-8-2020
                                        "</br>Shelter in Place: ",format(as.Date(SIP), "%m/%d/%Y"), #10-8-2020
                                        "</br>Bar/Restuarant Closure: ",format(as.Date(RB), "%m/%d/%Y")#, #4-7-2-20 #10-8-2020
                                        # "</br>Social Distancing: ",format(as.Date(SD), "%m/%d") #4-7-2-20
                         )
    )
    )
    
    suppressWarnings(isolate(p2 <- layout(p2, showlegend = FALSE )))
    suppressWarnings(isolate(p2 <- p2 %>% hide_colorbar()))
    
    #suppressWarnings(isolate(p2 %>% hide_colorbar()  ))
    #suppressWarnings(isolate(p2 <- colorbar(p2, title = "", tickvals = c(), ticktext =  c(), showscale=FALSE)))
    #suppressWarnings(isolate(p2 <- colorbar(p2, title = "Percentile Rank", tickvals = c(25, 50, 75, 100), ticktext =  c("25th","50th","75th","100th"), len = 0.6)))
    
    suppressWarnings(isolate(event_register(p2, "plotly_click")))
    
    
    
  })
  
  
  
  ######################################################################
  #  US Map -- dynamically update US Map so we don't have to redraw every time
  ######################################################################
  
  # If we see a change in the plot variable (e.g. cases, deaths, cases per million, etc.) -- change the fill color based on these values
  observeEvent(plotVarsUSmap()[["plotVar"]],{
    
    #phdva
    # print(plotVarsUSmap()[["plotVar"]])
    # print(buildStates()$colorMap)
    # print(buildStates()$value)
    
    # need to sort alphabetically by state (that is how the traces are drawn)
    for(i in 1:length(sort(buildStates()$state_name))){ #6-13-2020
      plotlyProxy("usaPlot", session) %>%
        plotlyProxyInvoke("restyle",list(fillcolor = buildStates()$colorMap[which(buildStates()$state_name == sort(buildStates()$state_name)[i])]),(i-1))
    }
  })
  
  # If we see a change in the selected state -- the state which we draw a teal border around
  observeEvent(input$cState,{
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    # Get all the states in order
    allStates = sort(buildStates()$state_name)
    # Start the redraw
    plotlyProxy("usaPlot", session) %>%
      # first make all the states have a grey border
      plotlyProxyInvoke("restyle",list(line.color = "#7f7f7f",
                                       line.width = 1)) %>%
      # then draw the cyab border around the correct state
      plotlyProxyInvoke("restyle",list(line.color = "#00FFFF",
                                       line.fillcolor = "#00FFFF",
                                       line.width = 3),(which(allStates==input$cState)-1))
  })
  
  # If we see a change in the selected date, we need to update the hover text
  observeEvent(input$daterange1,{
    # Pull in the state dataframe for the selected range and order by state alphabetically (this is how they are plotted)
    dat <- buildStates()[order(buildStates()$state_name),]
    plotlyProxy("usaPlot", session) %>%
      # plotlyProxyInvoke("restyle",list(text = paste0("Cases: ",dat$cases)))
      plotlyProxyInvoke("restyle",list(text = paste0('</br>State: ', str_to_title(dat$state_name),
                                                     '</br>Total ICU Beds: ', format(round(dat$ICUbeds, digits = 1),big.mark=",",scientific=FALSE),
                                                     '</br>Estimated ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$icu_bed_occ, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                                     '</br>Fraction ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$perc_icu_occ, digits = 1),big.mark=",",scientific=FALSE), "%", #10-8-2020
                                                     "</br>Doubling time (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$loessN, digits = 1),big.mark=",",scientific=FALSE), " days", #10-8-2020
                                                     '</br>Cases (', format(input$daterange1[1], "%m/%d/%Y"), ' - ', format(input$daterange1[2], "%m/%d/%Y"), '): ', format(dat$cases,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>Deaths (",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(dat$deaths,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>Cases Per Million (",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$casesPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>Deaths Per Million (",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$deathsPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Cases (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(dat$NewCases,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Deaths (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(dat$NewDeaths,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Cases Per Million (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$NewCasesPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Deaths Per Million (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$NewDeathsPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>State of Emergency: ",format(as.Date(dat$SE), "%m/%d/%Y"), #10-8-2020
                                                     "</br>School Closure: ",format(as.Date(dat$CS), "%m/%d/%Y"), #10-8-2020
                                                     "</br>Shelter in Place: ",format(as.Date(dat$SIP), "%m/%d/%Y"), #10-8-2020
                                                     "</br>Bar/Restuarant Closure: ",format(as.Date(dat$RB), "%m/%d/%Y")#, #4-7-2-20 #10-8-2020
                                                     # "</br>Social Distancing: ",format(as.Date(dat$SD), "%m/%d") #4-7-2-20
      )))
  })
  
  
  ######################################################################
  #  State Map
  ######################################################################
  
  output$stateMapPlot <- renderPlotly({
    
    # Handle when max date value is selected: 6-23-2020
    validate(
      need(input$daterange1[[2]] != max(countyDat$date), 'end date selected is beyond available data.')
    )
    
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    
    # Filter the date range for the data to show on the state map
    # Pull in the state data
    req(countyDat)
    # Join the state spatial data with the covid-19 data
    # dat1 = countyDat[which(countyDat$state == input$cState & countyDat$date == defaultDate1),]
    dat1 = countyDat[which(countyDat$state == isolate(input$cState) & countyDat$date == isolate(input$daterange1[1])),]
    # dat = countyDat[which(countyDat$state == input$cState & countyDat$date == defaultDate2),]
    dat = countyDat[which(countyDat$state == isolate(input$cState) & countyDat$date == isolate(input$daterange1[2])),]
    
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
    
    # Scale to percentile
    # DA added
    valUse <- pull(dat[,defaultValPlot])
    naIx <- which(is.na(valUse))
    if(length(naIx) > 0){
      valUse[-naIx] <- perc.rank(valUse[-naIx])
    } else{
      valUse <- perc.rank(valUse)
    }
    dat$value = valUse
    dat$value = dat$value*100
    # #Old
    # dat$value = perc.rank(pull(dat[,defaultValPlot]))
    # dat$value = dat$value*100
    
    # Get the county boundaries
    req(counties_sf)
    counties_sf_state <- subset(counties_sf, state_name == input$cState)
    dat <- left_join(counties_sf_state, dat, by = c("county_fips" = "fips"))
    
    # Rename columns for plotting
    dat <- dat %>%
      mutate(State = state_name) %>%
      mutate(County = county_name) %>%
      # Capitalize the county and sate names
      mutate(County = str_to_title(County)) %>%
      mutate(State = str_to_title(State))
    
    
    # Do state projection
    # Can't do projection for alaska or hawaii; nebraska looks better without (it gets smushed)
    if(!input$cState %in% c("Alaska","Hawaii")){
      
      if(input$cState %in% c("Nebraska","Indiana", "South Carolina","Delaware","District of Columbia")){
        
        statesLimits <- map_data("state")
        statesLimits = statesLimits[which(statesLimits$region == tolower(input$cState)),]
        minLat = min(statesLimits$lat); maxLat = max(statesLimits$lat); minLon = min(statesLimits$long); maxLon = max(statesLimits$long)
        midLat = (maxLat + minLat)/2; midLon = (maxLon + minLon)/2
        
        dat <- dat %>%
          st_transform(crs = paste0("+proj=aea +lat_1=",minLat," +lat_2=",maxLat," +lat_0=",midLat," +lon_0=",midLon," +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
        
      } else{
        if(input$cState %in% c("Massachusetts","Washington","North Dakota","Ohio","New Mexico","Minnesota","Mississippi","Florida","Alabama")){
          # Need to fix some states (they get squished)
          if(input$cState %in% c("Massachusetts")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "island")
          }
          if(input$cState %in% c("Washington","North Dakota","Ohio")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "south")
          }
          if(input$cState %in% c("New Mexico")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "east")
          }
          if(input$cState %in% c("Minnesota")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "north")
          }
          if(input$cState %in% c("Mississippi","Florida")){
            planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"), plane_id = "west")
          }
          if(input$cState %in% c("Alabama")){
            planeUse = "+proj=aea +lat_1=30.24071 +lat_2=35.01345 +lat_0=32.318231 +lon_0=-86.68851 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
          }
        }
        else{
          planeUse = state_plane(unique(dat$state_abbv), type = c("proj4"))
        }
        dat <- dat %>%
          st_transform(crs = planeUse)
      }
    }
    # add a color mapping to allow us to recolor the maps without have to redraw them
    if(length(dat$value) == 1){
      dat$colorMap = "#e6550d"
    } else{
      if(length(which(is.infinite(dat$value)))>0){  #6-13-2020
        colorCuts <- cut(dat$value, breaks = seq(min(dat$value[-which(is.infinite(dat$value))],na.rm = T), max(dat$value[-which(is.infinite(dat$value))],na.rm = T), len = 100), include.lowest = TRUE) #6-13-2020
        dat$colorMap <- colorRampPalette(colors = c("#ffffff", "#ffbe87", "#e6550d"))(99)[colorCuts]
      } else{
        colorCuts <- cut(dat$value, breaks = seq(min(dat$value,na.rm = T), max(dat$value,na.rm = T), len = 100), include.lowest = TRUE) #6-13-2020
        dat$colorMap <- colorRampPalette(colors = c("#ffffff", "#ffbe87", "#e6550d"))(99)[colorCuts]
      }
    }
    
    
    
    # DA added
    # naIx <- which(is.na(pull(dat[,defaultValPlot])))
    naIx <- which(is.na(dat$colorMap))
    if(length(naIx) > 0){
      dat$colorMap[naIx] <- "#BFBFBF"
    }
    
    
    # Create graph Title
    output$state_map_title = renderText({ plotVarsStateMap()[["annoTitle"]] })
    
    #Silence the the following warning: No trace type specified: Based on info supplied, a 'scatter' trace seems appropriate. Read more about this trace type -> https://plot.ly/r/reference/#scatter
    isolate(p3 <- plot_ly())
    isolate(p3 <- add_sf(p3, data = dat,
                         type = "scatter", #6-13-2020
                         inherit = T,
                         colors = colorPallette,
                         color = ~value,
                         split = ~county_fips,
                         key = ~county_fips,
                         alpha = 1,
                         stroke = I("gray50"),
                         span = I(1),
                         hoverinfo = "text",
                         hoveron = "fills",
                         text = ~paste0('</br>State: ', str_to_title(State),
                                        '</br>County: ', str_to_title(County),
                                        "</br>Doubling time (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(loessN, digits = 1),big.mark=",",scientific=FALSE), " days", #10-8-2020
                                        '</br>Total ICU Beds: ', format(round(ICUbeds, digits = 1),big.mark=",",scientific=FALSE),
                                        '</br>Estimated ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(icu_bed_occ, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        '</br>Fraction ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(perc_icu_occ, digits = 1),big.mark=",",scientific=FALSE), "%", #10-8-2020
                                        '</br>Cases (', format(isolate(input$daterange1[1]), "%m/%d/%Y"), ' - ', format(isolate(input$daterange1[2]), "%m/%d/%Y"), '): ', format(cases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths (",format(isolate(input$daterange1[1]), "%m/%d/%Y")," - ",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(deaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Cases Per Million (",format(isolate(input$daterange1[1]), "%m/%d/%Y")," - ",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(casesPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths Per Million (",format(isolate(input$daterange1[1]), "%m/%d/%Y")," - ",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(deathsPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(NewCases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases Per Million (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(NewCasesPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths Per Million (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(NewDeathsPerMillion, digits = 1), big.mark=",",scientific=FALSE) #10-8-2020
                         )
    ))
    isolate(p3 <- add_sf(p3,data = dat[which(is.na(dat$value)),],
                         inherit = F,
                         type = "scatter", #6-13-2020
                         split = ~county_fips,
                         key = ~county_fips,
                         color = I("gray75"),
                         alpha = 1,
                         stroke = I("gray50"),
                         span = I(1),
                         hoverinfo = "text",
                         hoveron = "fills",
                         text = ~paste0('</br>State: ', str_to_title(State),
                                        '</br>County: ', str_to_title(County),
                                        "</br>Doubling time (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(loessN, digits = 1),big.mark=",",scientific=FALSE), " days", #10-8-2020
                                        '</br>Total ICU Beds: ', format(round(ICUbeds, digits = 1),big.mark=",",scientific=FALSE),
                                        '</br>Estimated ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(icu_bed_occ, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        '</br>Fraction ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(perc_icu_occ, digits = 1),big.mark=",",scientific=FALSE), "%", #10-8-2020
                                        '</br>Cases (', format(isolate(input$daterange1[1]), "%m/%d/%Y"), ' - ', format(isolate(input$daterange1[2]), "%m/%d/%Y"), '): ', format(cases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths (",format(isolate(input$daterange1[1]), "%m/%d/%Y")," - ",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(deaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Cases Per Million (",format(isolate(input$daterange1[1]), "%m/%d/%Y")," - ",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(casesPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>Deaths Per Million (",format(isolate(input$daterange1[1]), "%m/%d/%Y")," - ",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(deathsPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(NewCases,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(NewDeaths,big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Cases Per Million (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(NewCasesPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                        "</br>New Deaths Per Million (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(NewDeathsPerMillion, digits = 1), big.mark=",",scientific=FALSE) #10-8-2020
                         )
    ))
    suppressWarnings(isolate(p3 <- layout(p3, showlegend = FALSE)))
    suppressWarnings(isolate(p3 <- p3 %>% hide_colorbar()))
    suppressWarnings(p3)
    
    #partial_bundle(local = FALSE) %>%
    #plotly_mod_dep() %>%
    #toWebGL()
    # toWebGL(p3)
    
  })
  
  ######################################################################
  #  State Map -- dynamically update State Map so we don't have to redraw every time
  ######################################################################
  
  #uncomment
  # If we see a change in the plot variable (e.g. cases, deaths, cases per million, etc.) -- change the fill color based on these values
  observeEvent(plotVarsStateMap()[["plotVar"]],{
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    # need to sort alphabetically by county (that is how the traces are drawn)
    for(i in 1:length(sort(stateProjection()$county_fips))){
      plotlyProxy("stateMapPlot", session) %>%
        plotlyProxyInvoke("restyle",list(fillcolor = stateProjection()$colorMap[which(stateProjection()$county_fips == sort(stateProjection()$county_fips)[i])]),(i-1))
    }
  })
  
  # If we see a change in the selected date, we need to update the hover text
  observeEvent(input$daterange1,{
    # Add exception if no state is selected
    validate(
      need(input$cState != '', 'Please choose a state.')
    )
    # Pull in the state dataframe for the selected range and order by state alphabetically (this is how they are plotted)
    dat <- stateProjection()[order(stateProjection()$county_fips),]
    plotlyProxy("stateMapPlot", session) %>%
      plotlyProxyInvoke("restyle",list(text = paste0('</br>State: ', str_to_title(dat$State),
                                                     '</br>County: ', str_to_title(dat$County),
                                                     "</br>Doubling Time (",format(isolate(input$daterange1[2]), "%m/%d/%Y"),"): ", format(round(dat$loessN, digits = 1), big.mark=",",scientific=FALSE), " days", #10-8-2020
                                                     '</br>Total ICU Beds: ', format(round(dat$ICUbeds, digits = 1),big.mark=",",scientific=FALSE),
                                                     '</br>Estimated ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$icu_bed_occ, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                                     '</br>Fraction ICU Beds Needed (',format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$perc_icu_occ, digits = 1),big.mark=",",scientific=FALSE), "%", #10-8-2020
                                                     '</br>Cases (', format(input$daterange1[1], "%m/%d/%Y"), ' - ', format(input$daterange1[2], "%m/%d/%Y"), '): ', format(dat$cases,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>Deaths (",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(dat$deaths,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>Cases Per Million (",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$casesPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>Deaths Per Million (",format(input$daterange1[1], "%m/%d/%Y")," - ",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$deathsPerMillion, digits = 1),big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Cases (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(dat$NewCases,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Deaths (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(dat$NewDeaths,big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Cases Per Million (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$NewCasesPerMillion, digits = 1), big.mark=",",scientific=FALSE), #10-8-2020
                                                     "</br>New Deaths Per Million (",format(input$daterange1[2], "%m/%d/%Y"),"): ", format(round(dat$NewDeathsPerMillion, digits = 1), big.mark=",",scientific=FALSE)))) #10-8-2020
  })
  
  
  ######################################################################
  #  Counties Data Table
  ######################################################################
  # output$countyTable <- renderDataTable(plotTable)
}

######################################################################
#    Run App  
######################################################################

# new -- to update the url
#options(shiny.port = 8888)
#options(shiny.host = "0.0.0.0")
#options(shiny.launch.browser = FALSE)

enableBookmarking("url")
shinyApp(ui = ui, server = server)