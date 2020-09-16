######################################################################
######################################################################
#    PreProcessing
######################################################################
######################################################################

######################################################################
# Setup Notebook
######################################################################

# Load Libraries
library(shiny)
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
territories = c("Guam", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands", "American Samoa")
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
  dplyr::group_by(State) %>% 
  dplyr::summarise(value = sum(value))

######################################################################
# Get the number of new cases and deaths per day
######################################################################
# New cases per day
countyDat <- countyDat %>% 
  group_by(county,state,fips) %>% 
  dplyr::mutate(NewCases = cases - lag(cases, default = 0, order_by = date))

#New deaths per day
countyDat <- countyDat %>% 
  group_by(county,state,fips) %>% 
  dplyr::mutate(NewDeaths = deaths - lag(deaths, default = 0, order_by = date))

#Now for the states
stateDat <- stateDat %>% 
  group_by(state) %>% 
  dplyr::mutate(NewCases = cases - lag(cases, default = 0, order_by = date))
stateDat <- stateDat %>% 
  group_by(state) %>% 
  dplyr::mutate(NewDeaths = deaths - lag(deaths, default = 0, order_by = date))

######################################################################
# Join the US Census data with the NY Times data
######################################################################
# Join Data
countyDat <- left_join(countyDat, popEst, by = c("fips" = "GEOID"))

countyDat <- countyDat %>%
  filter(!is.na(value)) # Here we are removing all the "Unknown" -- need to update this.... potentially use some of Paul's suggestions?

# Get cases and deaths per million population
countyDat <- countyDat %>%
  dplyr::mutate(casesPerMillion = (cases/value)*1000000) %>%
  dplyr::mutate(deathsPerMillion = (deaths/value)*1000000) %>%
  dplyr::mutate(NewCasesPerMillion = (NewCases/value)*1000000) %>%
  dplyr::mutate(NewDeathsPerMillion = (NewDeaths/value)*1000000)

# Now do for the states
stateDat <- left_join(stateDat, statePopEst, by = c("state" = "State"))
# Get cases and deaths per million population
stateDat <- stateDat %>%
  dplyr::mutate(casesPerMillion = (cases/value)*1000000) %>%
  dplyr::mutate(deathsPerMillion = (deaths/value)*1000000) %>%
  dplyr::mutate(NewCasesPerMillion = (NewCases/value)*1000000) %>%
  dplyr::mutate(NewDeathsPerMillion = (NewDeaths/value)*1000000)

######################################################################
# Set t = 0 to the first observed case in each county
######################################################################
# Set t=0 to the data of >= 10 cases
suppressWarnings( # this is noisy for N/As
  time_zero <- countyDat %>%
    group_by(state, county) %>%
    dplyr::summarise(first_case = min(date[which(cases>=10)])) %>%
    ungroup
)


# Set a new column for the time elapsed between the date column and the t=0 date for each row
countyDat <- countyDat %>%
  left_join(time_zero, by = c("state", "county")) %>%
  dplyr::mutate(time = as.numeric(date - first_case))

######################################################################
# State-mandated events -- mapped by Vivek, Travis, and Arman
######################################################################
# Read in the events mapped by Vivek, Travis and Arman
stateEvents <- read_csv(file = "./DataFiles/state-events.csv")
# stateEvents <- read_csv(file = "./LiveShiny/DataFiles/state-events.csv")

# Spread the data so we get an event in each column and the relevant date (if available) as the value
stateEvents <- stateEvents %>%
  pivot_wider(names_from = Event, values_from = Date)

# Join the state counts data with the state events data
stateDat <- stateDat %>%
  left_join(stateEvents, by = c("state" = "State_Name"))

######################################################################
# Calculated Doubling Time (Counties) -- Implemented by Travis Zack
######################################################################
#TRAVIS ZACK###
dbl_df <- data.frame(county=as.character(),state=as.character(),cur_double=as.numeric())
# Initialize parameters
min_cases_tot <- 100
min_cases_cur <- 10
all_states <- unique(countyDat$state)
rollnum <- 7
weights <- 0.9^(rollnum:1)
maxdouble <- 14
i <-1
countyDat$double <- as.double(NA)

for (j in 1:length(all_states)){
  statefocus <- all_states[j]
  idx_state <- which(countyDat$state == statefocus)
  caseDatastate <- countyDat[idx_state,]
  caseDatastate$double <- as.double(NA)
  all_counties <- unique(caseDatastate$county)
  n <- length(all_counties)
  keep_counties <- unique(caseDatastate[which(caseDatastate$cases>=min_cases_tot),]$county)
  most_recent_dbl_df <- data.frame(county=all_counties,state=rep(statefocus,n,1),cur_double=rep(as.double(NA),n,1))
  for (i in 1:length(all_counties)){
    idx_cnty <- which(caseDatastate$county==all_counties[i])
    max_cnty <- max(caseDatastate$cases[idx_cnty],na.rm=TRUE)
    if (max_cnty>=min_cases_tot){
      county_focus = c(all_counties[i])
      cnty_cur <- caseDatastate[idx_cnty,]
      #This is to drop all dates with cumulative cases less than some value to remove the really
      #unstable stuff at the beginning of the growth curves.... Worth fiddling with
      bad_idx <- which(cnty_cur$cases<=min_cases_tot)
      resultweighted <- roll_lm(as.integer(cnty_cur$date), log2(cnty_cur$cases), rollnum, weights)
      doubling <- 1/resultweighted$coefficients[,2]
      #doubling[is.na(doubling)] <- 0
      rolldata <- data.frame(double = doubling,
                             date = cnty_cur$date )
      #set extreme values as Na
      if (is.na(any(rolldata$double>1e8))==FALSE) {
        rolldata$double[which(rolldata$double >1e8)] <- as.double(NA)
      }
      if (is.na(any(rolldata$double< -1e8))==FALSE) {
        rolldata$double[which(rolldata$double < -1e8)] <- as.double(NA)
      }
      rolldata$double[bad_idx] <- as.double(NA)
      #interpolate NA to average of surrounding values
      rolldata$double <- (na.locf(rolldata$double,na.rm=FALSE) + na.locf(rolldata$double,fromLast=TRUE,na.rm=FALSE))/2
      caseDatastate$double[idx_cnty] <- rolldata$double
      most_recent_dbl_df$cur_double[i] = tail(rolldata$double,n=1)
    }
    countyDat$double[idx_state] <- caseDatastate$double
  }
  dbl_df <- rbind(dbl_df,most_recent_dbl_df)
}
#dbl_df is a dataframe with just the most recent doubling time estimates for easy grabing
#instead of adding the "double" column to countyDat, created countyDat1 so I didnt mess anything else up
#####END OF DOUBLING TIME CODE##################################

######################################################################
# Calculated Doubling Time (States) -- Implemented by Doug (based on Travis' code)
######################################################################
dbl_df_state <- data.frame(state=as.character(),cur_double=as.numeric())
# Initialize parameters
min_cases_tot <- 100
min_cases_cur <- 10
all_states <- unique(stateDat$state)
rollnum <- 7
weights <- 0.9^(rollnum:1)
maxdouble <- 14
i <-1
stateDat$double <- as.double(NA)

caseDatastate <- stateDat
caseDatastate$double <- as.double(NA)
all_states <- unique(caseDatastate$state)
n <- length(all_states)
keep_states <- unique(caseDatastate[which(caseDatastate$cases>=min_cases_tot),]$state)
most_recent_dbl_df_state <- data.frame(state=all_states,cur_double=rep(as.double(NA),n,1))
for (i in 1:length(all_states)){
  idx_state <- which(caseDatastate$state==all_states[i])
  max_state <- max(caseDatastate$cases[idx_state],na.rm=TRUE)
  if (max_state>=min_cases_tot){
    state_focus = c(all_states[i])
    state_cur <- caseDatastate[idx_state,]
    #This is to drop all dates with cumulative cases less than some value to remove the really
    #unstable stuff at the beginning of the growth curves.... Worth fiddling with
    bad_idx <- which(state_cur$cases<=min_cases_tot)
    resultweighted <- roll_lm(as.integer(state_cur$date), log2(state_cur$cases), rollnum, weights)
    doubling <- 1/resultweighted$coefficients[,2]
    #doubling[is.na(doubling)] <- 0
    rolldata <- data.frame(double = doubling,
                           date = state_cur$date )
    #set extreme values as Na
    if (is.na(any(rolldata$double>1e8))==FALSE) {
      rolldata$double[which(rolldata$double >1e8)] <- as.double(NA)
    }
    if (is.na(any(rolldata$double< -1e8))==FALSE) {
      rolldata$double[which(rolldata$double < -1e8)] <- as.double(NA)
    }
    rolldata$double[bad_idx] <- as.double(NA)
    #interpolate NA to average of surrounding values
    rolldata$double <- (na.locf(rolldata$double,na.rm=FALSE) + na.locf(rolldata$double,fromLast=TRUE,na.rm=FALSE))/2
    caseDatastate$double[idx_state] <- rolldata$double
    most_recent_dbl_df_state$cur_double[i] = tail(rolldata$double,n=1)
  }
  stateDat$double[idx_state] <- caseDatastate$double[idx_state]
}
dbl_df_state <- rbind(dbl_df_state,most_recent_dbl_df_state)
#dbl_df is a dataframe with just the most recent doubling time estimates for easy grabing
#instead of adding the "double" column to countyDat, created countyDat1 so I didnt mess anything else up
#####END OF DOUBLING TIME CODE#################################

######################################################################
# ICU BED Occupancy (Counties) -- Implemented by Travis Zack
######################################################################
#Based on cases over preceding period

#importing ICU bed data
ICU <- read.csv('./DataFiles/data-ICU-beds.txt')

# Fix NYC
nycCounties = c("New York", "Kings", "Queens", "Bronx", "Richmond")
nycCounts = colSums(ICU[which(ICU$County %in% nycCounties & ICU$State == "New York"),c("ICU.Beds","Total.Population","Population.Aged.60.","Percent.of.Population.Aged.60.")])
nycCounts = data.frame(State = "New York", County = "New York City", ICU.Beds = nycCounts["ICU.Beds"], Total.Population = nycCounts["Total.Population"],
                       Population.Aged.60. = nycCounts["Population.Aged.60."], Percent.of.Population.Aged.60. = as.double(NA), Residents.Aged.60..Per.Each.ICU.Bed = as.double(NA))
ICU = ICU[-which(ICU$State %in% "New York" & ICU$County %in% nycCounties),]
ICU = rbind(ICU,nycCounts)

# Fix KC Mo -- here, combine Cass, Clay, Jackson and Platte with KC
kcCounties = c("Cass", "Clay", "Jackson", "Platte")
kcCounts = colSums(ICU[which(ICU$County %in% kcCounties & ICU$State == "Missouri"),c("ICU.Beds","Total.Population","Population.Aged.60.","Percent.of.Population.Aged.60.")])
kcCounts = data.frame(State = "Missouri", County = "Kansas City", ICU.Beds = kcCounts["ICU.Beds"], Total.Population = kcCounts["Total.Population"],
                      Population.Aged.60. = kcCounts["Population.Aged.60."], Percent.of.Population.Aged.60. = as.double(NA), Residents.Aged.60..Per.Each.ICU.Bed = as.double(NA))
ICU = ICU[-which(ICU$State %in% "Missouri" & ICU$County %in% kcCounties),]
ICU = rbind(ICU,nycCounts)

##DOUG --> Can you do this smarter :) Just want to add the ICU bed data for each county to our table
#Also have to refigure out the NYC thing likely
countyDat$ICUbeds <- as.double(NA)
countyDat$icu_bed_occ <- as.double(NA)
countyDat$tot_icu_bed_occ <- as.double(NA)
for (i in 1:nrow(ICU)){
  idx_cur <- which((countyDat$state==ICU$State[i]) & (countyDat$county==ICU$County[i]))
  if (length(idx_cur)>0){
    countyDat$ICUbeds[idx_cur] <- ICU$ICU.Beds[i]
  }
  
}

## Vivek Comment: Assumes 4.4% of new cases are admitted,
## 30% of admitted patients escalate to the ICU,
## and all ICU patients spend 9 days (time to discharge or death)

icu_los <- 10
# hosp_frac <-0.044
# icu_frac <- 0.3
# hosp_frac <-0.103
# hosp_frac <-0.127 #8-19-2020
# hosp_frac <-0.033
# icu_frac <- 0.4 #8-14-2020 #8-19-2020

# hosp_frac <- 0.0626 #8-19-2020
# hosp_frac <- 0.0826 #8-19-2020
hosp_frac <- 0.0558 #9-15-2020
icu_frac <- 0.22908 #9-15-2020
# icu_frac <- 0.2964 #8-19-2020
# icu_frac <- 0.24198 #8-19-2020

# icu_frac <- 2.380952 #~42 percent of patients in the ICU die #8-14-2020 - https://www.healthline.com/health-news/covid-19-mortality-is-going-down-in-icus-what-this-means-for-the-pandemic#The-mortality-rate-is-dropping,-but-its-still-high
min_cases <- 100

all_states <- unique(countyDat$state)
# start_time <- Sys.time()
for (j in 1:length(all_states)){
  statefocus <- all_states[j]
  idx_state <- which(countyDat$state == statefocus)
  caseDatastate <- countyDat[idx_state,]
  caseDatastate$icu_bed_occ <- as.double(NA)
  caseDatastate$tot_icu_bed_occ <- as.double(NA)
  all_counties <- unique(caseDatastate$county)
  n <- length(all_counties)
  keep_counties <- unique(caseDatastate[which(caseDatastate$cases>=min_cases),]$county)
  most_recent_dbl_df <- data.frame(county=all_counties,state=rep(statefocus,n,1),cur_double=rep(as.double(NA),n,1))
  for (i in 1:length(all_counties)){
    idx_cnty <- which(caseDatastate$county==all_counties[i])
    cnty_cur <- caseDatastate[idx_cnty,]
    if (nrow(cnty_cur)>(icu_los+1)){
      # Use NewCases for how many patients currently in the ICU
      vec_st <- cnty_cur$NewCases[1:(icu_los+1)] #8-14-2020
      # vec_st <- cnty_cur$NewDeaths[1:(icu_los+1)] #8-14-2020
      # vec_st <- cnty_cur$cases[1:(icu_los+1)]
      # cnty_cur$icu_bed_occ = rollsum(x = cnty_cur$cases, icu_los, align = "right", fill = as.double(NA))*hosp_frac*icu_frac
      cnty_cur$icu_bed_occ = rollsum(x = cnty_cur$NewCases, icu_los, align = "right", fill = as.double(NA))*hosp_frac*icu_frac #8-14-2020
      # cnty_cur$icu_bed_occ = rollsum(x = cnty_cur$NewDeaths, icu_los, align = "right", fill = as.double(NA))*icu_frac #8-14-2020
      vec_st_mut <- vec_st
      for (k in 1:(icu_los-1)){
        vec_st_mut[k] <- sum(vec_st[1:k])*hosp_frac*icu_frac #8-14-2020
        # vec_st_mut[k] <- sum(vec_st[1:k])*icu_frac #8-14-2020
      }
      cnty_cur$icu_bed_occ[1:(icu_los-1)] <- vec_st_mut[1:(icu_los-1)]
      
      # If we see a jump over 200% at t = 2 days with reference of t = 1 days and then back down under 100% at t = 3 days with t = 1 days as reference, set to N/A and interpolate to avg of surrounding values
      pctIncrease = cnty_cur %>%
        dplyr::mutate(pctIncrease1 = icu_bed_occ / lag(icu_bed_occ, n = 1, default = first(icu_bed_occ))) %>%
        dplyr::mutate(pctIncrease2 = icu_bed_occ / lag(icu_bed_occ, n = 2, default = first(icu_bed_occ)))
      
      #set extreme values as Na (peaks)
      if (is.na(any(abs(pctIncrease$pctIncrease1)<0.5 & abs(pctIncrease$pctIncrease2)>=1))==FALSE) {
        cnty_cur$icu_bed_occ[which(abs(pctIncrease$pctIncrease1)<0.5 & abs(pctIncrease$pctIncrease2)>=1) - 1] <- as.double(NA)
      }
      
      #interpolate NA to average of surrounding values
      cnty_cur$icu_bed_occ <- (na.locf(cnty_cur$icu_bed_occ,na.rm=FALSE) + na.locf(cnty_cur$icu_bed_occ,fromLast=TRUE,na.rm=FALSE))/2
      
      caseDatastate$icu_bed_occ[idx_cnty] <- cnty_cur$icu_bed_occ
      
      # Use cases for cumulative number of patients in the ICU
      cnty_cur$tot_icu_bed_occ = (cnty_cur$cases)*hosp_frac*icu_frac  #8-14-2020
      # cnty_cur$tot_icu_bed_occ = (cnty_cur$cases)*icu_frac  #8-14-2020
      
      # vec_st <- cnty_cur$cases[1:(icu_los+1)]
      # cnty_cur$tot_icu_bed_occ = rollsum(x = cnty_cur$cases, icu_los, align = "right", fill = as.double(NA))*hosp_frac*icu_frac
      # vec_st_mut <- vec_st
      # for (k in 1:(icu_los-1)){
      #   vec_st_mut[k] <- sum(vec_st[1:k])*hosp_frac*icu_frac
      # }
      # cnty_cur$tot_icu_bed_occ[1:(icu_los-1)] <- vec_st_mut[1:(icu_los-1)]
      
      # If we see a jump over 200% at t = 2 days with reference of t = 1 days and then back down under 100% at t = 3 days with t = 1 days as reference, set to N/A and interpolate to avg of surrounding values
      pctIncrease = cnty_cur %>%
        dplyr::mutate(pctIncrease1 = tot_icu_bed_occ / lag(tot_icu_bed_occ, n = 1, default = first(tot_icu_bed_occ))) %>%
        dplyr::mutate(pctIncrease2 = tot_icu_bed_occ / lag(tot_icu_bed_occ, n = 2, default = first(tot_icu_bed_occ)))
      
      #set extreme values as Na (peaks)
      if (is.na(any(abs(pctIncrease$pctIncrease1)<0.5 & abs(pctIncrease$pctIncrease2)>=1))==FALSE) {
        cnty_cur$tot_icu_bed_occ[which(abs(pctIncrease$pctIncrease1)<0.5 & abs(pctIncrease$pctIncrease2)>=1) - 1] <- as.double(NA)
      }
      
      #interpolate NA to average of surrounding values
      cnty_cur$tot_icu_bed_occ <- (na.locf(cnty_cur$tot_icu_bed_occ,na.rm=FALSE) + na.locf(cnty_cur$tot_icu_bed_occ,fromLast=TRUE,na.rm=FALSE))/2
      
      caseDatastate$tot_icu_bed_occ[idx_cnty] <- cnty_cur$tot_icu_bed_occ
    }
  }
  countyDat$icu_bed_occ[idx_state] <- caseDatastate$icu_bed_occ
  countyDat$tot_icu_bed_occ[idx_state] <- caseDatastate$tot_icu_bed_occ
}
countyDat$icu_bed_occ[which(countyDat$icu_bed_occ < 0)] <- 0
# Convert to percent occupied
countyDat$perc_icu_occ <- 100*(countyDat$icu_bed_occ/countyDat$ICUbeds)
# Change infinite values to NA
countyDat$perc_icu_occ[is.infinite(countyDat$perc_icu_occ)] <- as.double(NA)


# end_time <- Sys.time()
# print(end_time-start_time)

######################################################################
# ICU BED Occupancy (States) -- Counties implemented by Travis -- states extended by Doug
######################################################################

# Get the beds per state
ICUstate <- ICU %>%
  group_by(State) %>%
  summarise(ICU.Beds = sum(ICU.Beds,na.rm = T))

##DOUG --> Can you do this smarter :) Just want to add the ICU bed data for each county to our table
#Also have to refigure out the NYC thing likely
stateDat$ICUbeds <- as.double(NA)
stateDat$icu_bed_occ <- as.double(NA)
stateDat$total_icu_bed_occ <- as.double(NA)
for (i in 1:nrow(ICUstate)){
  idx_cur <- which((stateDat$state==ICUstate$State[i]))
  if (length(idx_cur)>0){
    stateDat$ICUbeds[idx_cur] <- ICUstate$ICU.Beds[i]
  }
  
}

all_states <- unique(stateDat$state)
for (i in 1:length(all_states)){
  statefocus <- all_states[i]
  idx_state <- which(stateDat$state == statefocus)
  caseDatastate <- stateDat[idx_state,]
  # idx_state <- which(caseDatastate$county==all_counties[i])
  state_cur <- caseDatastate
  icuBedState <- caseDatastate$icu_bed_occ
  if (nrow(state_cur)>(icu_los+1)){
    # Use NewCases for how many patients currently in the ICU
    # vec_st <- state_cur$cases[1:(icu_los+1)]
    vec_st <- state_cur$NewCases[1:(icu_los+1)] #8-14-2020
    # vec_st <- state_cur$NewDeaths[1:(icu_los+1)] #8-14-2020
    # state_cur$icu_bed_occ = rollsum(x = state_cur$NewDeaths, icu_los, align = "right", fill = as.double(NA))*icu_frac #8-14-2020
    state_cur$icu_bed_occ = rollsum(x = state_cur$NewCases, icu_los, align = "right", fill = as.double(NA))*hosp_frac*icu_frac #8-14-2020
    # state_cur$icu_bed_occ = rollsum(x = state_cur$cases, icu_los, align = "right", fill = as.double(NA))*hosp_frac*icu_frac
    vec_st_mut <- vec_st
    for (k in 1:(icu_los-1)){
      vec_st_mut[k] <- sum(vec_st[1:k])*hosp_frac*icu_frac #8-14-2020
      # vec_st_mut[k] <- sum(vec_st[1:k])*icu_frac #8-14-2020
    }
    state_cur$icu_bed_occ[1:(icu_los-1)] <- vec_st_mut[1:(icu_los-1)]
    
    icuBedState <- state_cur$icu_bed_occ
    
    # Use cases for cumulative number of patients in the ICU
    state_cur$tot_icu_bed_occ = (state_cur$cases)*hosp_frac*icu_frac #8-14-2020
    # state_cur$tot_icu_bed_occ = (state_cur$cases)*icu_frac #8-14-2020
    # vec_st <- state_cur$cases[1:(icu_los+1)]
    # state_cur$total_icu_bed_occ = rollsum(x = state_cur$cases, icu_los, align = "right", fill = as.double(NA))*hosp_frac*icu_frac
    # vec_st_mut <- vec_st
    # for (k in 1:(icu_los-1)){
    #   vec_st_mut[k] <- sum(vec_st[1:k])*hosp_frac*icu_frac
    # }
    # state_cur$total_icu_bed_occ[1:(icu_los-1)] <- vec_st_mut[1:(icu_los-1)]
    
    totalIcuBedState <- state_cur$total_icu_bed_occ
  }
  # stateDat$icu_bed_occ[idx_state] <- caseDatastate$icu_bed_occ[idx_state]
  stateDat$icu_bed_occ[idx_state] <- icuBedState
  stateDat$total_icu_bed_occ[idx_state] <- totalIcuBedState
}


# Convert to percent occupied
stateDat$perc_icu_occ <- 100*(stateDat$icu_bed_occ/stateDat$ICUbeds)
# Change infinite values to NA
stateDat$perc_icu_occ[is.infinite(stateDat$perc_icu_occ)] <- as.double(NA)

# ######################################################################
# # ICU BED Occupancy (Counties) V2 -- Implemented by Doug
# ######################################################################
# # Based on this model: https://www.medrxiv.org/content/10.1101/2020.04.07.20056226v1.full.pdf
# # Based on cases over preceding period
# 
# #importing ICU bed data
# ICU <- read.csv('./DataFiles/data-ICU-beds.txt')
# 
# # Fix NYC
# nycCounties = c("New York", "Kings", "Queens", "Bronx", "Richmond")
# nycCounts = colSums(ICU[which(ICU$County %in% nycCounties & ICU$State == "New York"),c("ICU.Beds","Total.Population","Population.Aged.60.","Percent.of.Population.Aged.60.")])
# nycCounts = data.frame(State = "New York", County = "New York City", ICU.Beds = nycCounts["ICU.Beds"], Total.Population = nycCounts["Total.Population"],
#                        Population.Aged.60. = nycCounts["Population.Aged.60."], Percent.of.Population.Aged.60. = as.double(NA), Residents.Aged.60..Per.Each.ICU.Bed = as.double(NA))
# ICU = ICU[-which(ICU$State %in% "New York" & ICU$County %in% nycCounties),]
# ICU = rbind(ICU,nycCounts)
# 
# # Fix KC Mo -- here, combine Cass, Clay, Jackson and Platte with KC
# kcCounties = c("Cass", "Clay", "Jackson", "Platte")
# kcCounts = colSums(ICU[which(ICU$County %in% kcCounties & ICU$State == "Missouri"),c("ICU.Beds","Total.Population","Population.Aged.60.","Percent.of.Population.Aged.60.")])
# kcCounts = data.frame(State = "Missouri", County = "Kansas City", ICU.Beds = kcCounts["ICU.Beds"], Total.Population = kcCounts["Total.Population"],
#                       Population.Aged.60. = kcCounts["Population.Aged.60."], Percent.of.Population.Aged.60. = as.double(NA), Residents.Aged.60..Per.Each.ICU.Bed = as.double(NA))
# ICU = ICU[-which(ICU$State %in% "Missouri" & ICU$County %in% kcCounties),]
# ICU = rbind(ICU,nycCounts)
# 
# ##DOUG --> Can you do this smarter :) Just want to add the ICU bed data for each county to our table
# #Also have to refigure out the NYC thing likely
# countyDat$ICUbeds <- as.double(NA)
# countyDat$icu_bed_occ <- as.double(NA)
# for (i in 1:nrow(ICU)){
#   idx_cur <- which((countyDat$state==ICU$State[i]) & (countyDat$county==ICU$County[i]))
#   if (length(idx_cur)>0){
#     countyDat$ICUbeds[idx_cur] <- ICU$ICU.Beds[i]
#   }
#   
# }
# 
# ## Vivek Comment: Assumes 4.4% of new cases are admitted, 
# ## 30% of admitted patients escalate to the ICU, 
# ## and all ICU patients spend 9 days (time to discharge or death)
# 
# icu_los <- 9
# hosp_frac <-0.044
# icu_frac <- 0.3
# min_cases <- 100
# 
# dailyICUrate <- 0.03
# weeklyICUrate <- 0.0015
# 
# all_states <- unique(countyDat$state)
# # start_time <- Sys.time()
# for (j in 1:length(all_states)){
#   statefocus <- all_states[j]
#   idx_state <- which(countyDat$state == statefocus)
#   caseDatastate <- countyDat[idx_state,]
#   caseDatastate$icu_bed_occ <- as.double(NA)
#   all_counties <- unique(caseDatastate$county)
#   n <- length(all_counties)
#   keep_counties <- unique(caseDatastate[which(caseDatastate$cases>=min_cases),]$county)
#   most_recent_dbl_df <- data.frame(county=all_counties,state=rep(statefocus,n,1),cur_double=rep(as.double(NA),n,1))
#   for (i in 1:length(all_counties)){
#     idx_cnty <- which(caseDatastate$county==all_counties[i])
#     cnty_cur <- caseDatastate[idx_cnty,]
#     if (nrow(cnty_cur)>(icu_los+1)){
#       # vec_st <- cnty_cur$cases[1:(icu_los+1)]
#       
#       #ICU Daily Admissions: 0.137*DailyNewCases + 0.078*NewWeeklyCases
#       #ICU Daily Discharged: 0.397*DailyAdmitted4DaysAgo + 0.603*[0.144*(0.783*DailyAdmitted4DaysAgo + 0.217*DailyAdmitted15DaysAgo) + 0.856*(0.444*DailyAdmitted6DaysAgo + 0.556*DailyAdmitted20DaysAgo)]
#       
#       # Set negative new cases to 0 (this is due to issues with reporting)
#       cnty_cur$NewCases[which(cnty_cur$NewCases < 0)] <- 0
#       # Pad the front with 6 days of 0 cases (before the first case, there were no cases)
#       weeklyNewCases <- rollsum(x = c(rep(0,6),cnty_cur$NewCases), k = 7, align = "left")
#       # dailyAdmissions <- 0.137*cnty_cur$NewCases + 0.078+weeklyNewCases
#       dailyAdmissions <- dailyICUrate*cnty_cur$NewCases + weeklyICUrate*weeklyNewCases
#       # Pad the front with 4 days of 0 cases (before the first case, there were no cases)
#       dailyAdmissions4DaysPrior <- c(rep(0,4),dailyAdmissions[1:(length(dailyAdmissions)-4)])
#       dailyAdmissions6daysPrior <- c(rep(0,6),dailyAdmissions[1:(length(dailyAdmissions)-6)])
#       if(length(dailyAdmissions)>15){
#         dailyAdmissions15DaysPrior <- c(rep(0,15),dailyAdmissions[1:(length(dailyAdmissions)-15)])
#       } else{
#         dailyAdmissions15DaysPrior <- c(rep(0,length(dailyAdmissions)))
#       }
#       if(length(dailyAdmissions)>20){
#         dailyAdmissions20DaysPrior <- c(rep(0,20),dailyAdmissions[1:(length(dailyAdmissions)-20)])
#       } else{
#         dailyAdmissions20DaysPrior <- c(rep(0,length(dailyAdmissions)))
#       }
#       dailyDischarged <- 0.397*dailyAdmissions4DaysPrior + 0.603*(0.144*(0.783*dailyAdmissions4DaysPrior + 0.217*dailyAdmissions15DaysPrior) + 0.856*(0.444*dailyAdmissions6daysPrior + 0.556*dailyAdmissions20DaysPrior))
#       
#       for(z in 1:length(dailyAdmissions)){
#         if(z == 1){
#           dailyICU <- c(0)
#         } else{
#           toAdd <- dailyICU[z-1] + dailyAdmissions[z] - dailyDischarged[z]
#           if(toAdd<0){
#             toAdd <- 0
#           }
#           dailyICU <- c(dailyICU,toAdd)
#         }
#       }
#       
#       cnty_cur$icu_bed_occ <- dailyICU
#       
#       # # If we see a jump over 200% at t = 2 days with reference of t = 1 days and then back down under 100% at t = 3 days with t = 1 days as reference, set to N/A and interpolate to avg of surrounding values
#       # pctIncrease = cnty_cur %>%
#       #   mutate(pctIncrease1 = icu_bed_occ / lag(icu_bed_occ, n = 1, default = first(icu_bed_occ))) %>%
#       #   mutate(pctIncrease2 = icu_bed_occ / lag(icu_bed_occ, n = 2, default = first(icu_bed_occ)))
#       # 
#       # #set extreme values as Na (peaks)
#       # if (is.na(any(abs(pctIncrease$pctIncrease1)<0.5 & abs(pctIncrease$pctIncrease2)>=1))==FALSE) {
#       #   cnty_cur$icu_bed_occ[which(abs(pctIncrease$pctIncrease1)<0.5 & abs(pctIncrease$pctIncrease2)>=1) - 1] <- as.double(NA)
#       # }
#       # #interpolate NA to average of surrounding values
#       # cnty_cur$icu_bed_occ <- (na.locf(cnty_cur$icu_bed_occ,na.rm=FALSE) + na.locf(cnty_cur$icu_bed_occ,fromLast=TRUE,na.rm=FALSE))/2
#       # 
#       caseDatastate$icu_bed_occ[idx_cnty] <- cnty_cur$icu_bed_occ
#     }
#   }
#   countyDat$icu_bed_occ[idx_state] <- caseDatastate$icu_bed_occ
# }
# # Convert to percent occupied
# countyDat$perc_icu_occ <- 100*(countyDat$icu_bed_occ/countyDat$ICUbeds)
# # Change infinite values to NA
# countyDat$perc_icu_occ[is.infinite(countyDat$perc_icu_occ)] <- as.double(NA)
# 
# 
# ######################################################################
# # ICU BED Occupancy (States) -- V2 -- Implemented by Doug
# ######################################################################
# # Based on this model: https://www.medrxiv.org/content/10.1101/2020.04.07.20056226v1.full.pdf
# # Based on cases over preceding period
# 
# # Get the beds per state
# ICUstate <- ICU %>%
#   group_by(State) %>%
#   summarise(ICU.Beds = sum(ICU.Beds,na.rm = T))
# 
# ##DOUG --> Can you do this smarter :) Just want to add the ICU bed data for each county to our table
# #Also have to refigure out the NYC thing likely
# stateDat$ICUbeds <- as.double(NA)
# stateDat$icu_bed_occ <- as.double(NA)
# for (i in 1:nrow(ICUstate)){
#   idx_cur <- which((stateDat$state==ICUstate$State[i]))
#   if (length(idx_cur)>0){
#     stateDat$ICUbeds[idx_cur] <- ICUstate$ICU.Beds[i]
#   }
#   
# }
# 
# dailyICUrate <- 0.03
# weeklyICUrate <- 0.0015
# 
# all_states <- unique(stateDat$state)
# for (i in 1:length(all_states)){
#   statefocus <- all_states[i]
#   idx_state <- which(stateDat$state == statefocus)
#   caseDatastate <- stateDat[idx_state,]
#   # idx_state <- which(caseDatastate$county==all_counties[i])
#   state_cur <- caseDatastate
#   icuBedState <- caseDatastate$icu_bed_occ
#   if (nrow(state_cur)>(icu_los+1)){
#     # vec_st <- state_cur$cases[1:(icu_los+1)]
#     
#     #ICU Daily Admissions: 0.137*DailyNewCases + 0.078*NewWeeklyCases
#     #ICU Daily Discharged: 0.397*DailyAdmitted4DaysAgo + 0.603*[0.144*(0.783*DailyAdmitted4DaysAgo + 0.217*DailyAdmitted15DaysAgo) + 0.856*(0.444*DailyAdmitted6DaysAgo + 0.556*DailyAdmitted20DaysAgo)]
#     
#     # Set negative new cases to 0 (this is due to issues with reporting)
#     state_cur$NewCases[which(state_cur$NewCases < 0)] <- 0
#     # Pad the front with 6 days of 0 cases (before the first case, there were no cases)
#     weeklyNewCases <- rollsum(x = c(rep(0,6),state_cur$NewCases), k = 7, align = "left")
#     # dailyAdmissions <- 0.137*cnty_cur$NewCases + 0.078+weeklyNewCases
#     dailyAdmissions <- dailyICUrate*state_cur$NewCases + weeklyICUrate*weeklyNewCases
#     # Pad the front with 4 days of 0 cases (before the first case, there were no cases)
#     dailyAdmissions4DaysPrior <- c(rep(0,4),dailyAdmissions[1:(length(dailyAdmissions)-4)])
#     dailyAdmissions6daysPrior <- c(rep(0,6),dailyAdmissions[1:(length(dailyAdmissions)-6)])
#     if(length(dailyAdmissions)>15){
#       dailyAdmissions15DaysPrior <- c(rep(0,15),dailyAdmissions[1:(length(dailyAdmissions)-15)])
#     } else{
#       dailyAdmissions15DaysPrior <- c(rep(0,length(dailyAdmissions)))
#     }
#     if(length(dailyAdmissions)>20){
#       dailyAdmissions20DaysPrior <- c(rep(0,20),dailyAdmissions[1:(length(dailyAdmissions)-20)])
#     } else{
#       dailyAdmissions20DaysPrior <- c(rep(0,length(dailyAdmissions)))
#     }
#     dailyDischarged <- 0.397*dailyAdmissions4DaysPrior + 0.603*(0.144*(0.783*dailyAdmissions4DaysPrior + 0.217*dailyAdmissions15DaysPrior) + 0.856*(0.444*dailyAdmissions6daysPrior + 0.556*dailyAdmissions20DaysPrior))
#     
#     for(z in 1:length(dailyAdmissions)){
#       if(z == 1){
#         dailyICU <- c(0)
#       } else{
#         toAdd <- dailyICU[z-1] + dailyAdmissions[z] - dailyDischarged[z]
#         if(toAdd<0){
#           toAdd <- 0
#         }
#         dailyICU <- c(dailyICU,toAdd)
#       }
#     }
#     
#     state_cur$icu_bed_occ <- dailyICU
#     
#     icuBedState <- state_cur$icu_bed_occ
#     
#     # state_cur$icu_bed_occ = rollsum(x = state_cur$cases, icu_los, align = "right", fill = as.double(NA))*hosp_frac*icu_frac
#     # vec_st_mut <- vec_st
#     # for (k in 1:icu_los){
#     #   vec_st_mut[k] <- sum(vec_st[1:k])*hosp_frac*icu_frac
#     # }
#     # state_cur$icu_bed_occ[1:(icu_los+1)] <- vec_st_mut
#     
#     # caseDatastate$icu_bed_occ[idx_state] <- state_cur$icu_bed_occ
#     # icuBedState <- state_cur$icu_bed_occ
#   }
#   # stateDat$icu_bed_occ[idx_state] <- caseDatastate$icu_bed_occ[idx_state]
#   stateDat$icu_bed_occ[idx_state] <- icuBedState
# }
# 
# 
# # Convert to percent occupied
# stateDat$perc_icu_occ <- 100*(stateDat$icu_bed_occ/stateDat$ICUbeds)
# # Change infinite values to NA
# stateDat$perc_icu_occ[is.infinite(stateDat$perc_icu_occ)] <- as.double(NA)

######################################################################
# Get the county doubling time -- Implemented by Travis
######################################################################

dbl_df2 <- data.frame(county=as.character(),state=as.character(),cur_double=as.numeric())

#This is slow... we can speed it up
min_cases <- 10
df_1 <- as.data.frame(countyDat[1,], row.names = NULL,stringsasFactors=FALSE)
df_1$case_logratio <- NA
df_1$loessN <- NA
df_1$logr_idx <- NA

#This code takes your list of counties and removes dates that are NA and adds columns, smooths the curves, and creates
#the column Loess which is the smoothed doubling time.
for(state in unique(countyDat$state)){
  # print(state)
  counties <- unique(countyDat$county[which(countyDat$state == state)])
  n <- length(counties)
  most_recent_dbl_df <- data.frame(county=counties,state=rep(state,n,1),cur_double=rep(as.double(NA),n,1))
  for(county in counties){
    county_cur <- countyDat[countyDat$county==county & countyDat$state == state,]
    county_cur$case_logratio <- NA
    if(nrow(county_cur)>=2){
      county_cur$case_logratio[2:nrow(county_cur)] = diff(log2(county_cur$cases))
    }
    #for some reason have to renormalize my x axis out of dates or the smooth function fails
    county_cur$logr_idx <- c(1:nrow(county_cur))
    if(max(county_cur$cases) >= min_cases & length(which(county_cur$cases >= min_cases)) >= 8){
      county_cur <- county_cur[county_cur$cases>=min_cases,]
      county_cur <- county_cur[-1,]
      smooth_vals = predict(loess(case_logratio~logr_idx, data=county_cur))
      # Fix the scale
      smooth_vals = 1/smooth_vals
      # #set extreme values as Na
      if (is.na(any(smooth_vals>500))==FALSE) {
        smooth_vals[which(smooth_vals>500)] <- NA
      }
      #interpolate NA to average of surrounding values
      smooth_vals <- (na.locf(smooth_vals,na.rm=FALSE) + na.locf(smooth_vals,fromLast=TRUE,na.rm=FALSE))/2
      
      county_cur$loessN <- smooth_vals
      if(length(tail(smooth_vals[!is.na(smooth_vals)],n=1))==0){
        most_recent_dbl_df$cur_double[which(most_recent_dbl_df$county == county & most_recent_dbl_df$state == state)] = NA
      }else{
        most_recent_dbl_df$cur_double[which(most_recent_dbl_df$county == county & most_recent_dbl_df$state == state)] = tail(smooth_vals[!is.na(smooth_vals)],n=1)
      }
    }
    else{
      county_cur$loessN <- NA
    }
    df_1 <- rbind(df_1,data.frame(county_cur)) #changed
  }
  dbl_df2 <- rbind(dbl_df2,most_recent_dbl_df)
}
df_1 <- df_1[-1,]

countyDat <- left_join(countyDat,df_1[,c("date","county","state","fips","loessN")], by = c("date"="date","county"="county","state"="state","fips"="fips"))
# First value is sometimes negative -- set to N/A
countyDat$loessN[which(countyDat$loessN < 0)] <- NA
countyDat <- countyDat %>% ungroup()

# Fix negative values
dbl_df2$cur_double[which(dbl_df2$cur_double < 0)] <- NA
######################################################################
# Get the state doubling time -- Implemented by Travis
######################################################################

min_cases <- 25
df_2 <- as.data.frame(stateDat[1,], row.names = NULL,stringsasFactors=FALSE)
df_2$case_logratio <- NA
df_2$loessN <- NA
df_2$logr_idx <- NA

#This code takes your list of counties and removes dates that are NA and adds columns, smooths the curves, and creates
#the column Loess which is the smoothed doubling time.
for(state in unique(stateDat$state)){
  state_cur <- stateDat[stateDat$state == state,]
  state_cur$case_logratio <- NA
  if(nrow(state_cur)>=2){
    state_cur$case_logratio[2:nrow(state_cur)] = diff(log2(state_cur$cases))
  }
  #for some reason have to renormalize my x axis out of dates or the smooth function fails
  state_cur$logr_idx <- c(1:nrow(state_cur))
  if(max(state_cur$cases) >= min_cases & length(which(state_cur$cases >= min_cases)) >= 8){
    state_cur <- state_cur[state_cur$cases>=min_cases,]
    state_cur <- state_cur[-1,]
    smooth_vals = predict(loess(case_logratio~logr_idx, data=state_cur))
    # Take out of log scale
    smooth_vals = 1/smooth_vals
    # #set extreme values as Na
    if (is.na(any(smooth_vals>100))==FALSE) {
      smooth_vals[which(smooth_vals>100)] <- NA
    }
    #interpolate NA to average of surrounding values
    smooth_vals <- (na.locf(smooth_vals,na.rm=FALSE) + na.locf(smooth_vals,fromLast=TRUE,na.rm=FALSE))/2
    
    state_cur$loessN <- smooth_vals
  }
  else{
    state_cur$loessN <- NA
  }
  df_2 <- rbind(df_2,data.frame(state_cur)) #changed
}
df_2 <- df_2[-1,]

stateDat <- left_join(stateDat,df_2[,c("date","state","fips","loessN")], by = c("date"="date","state"="state","fips"="fips"))
# First value is sometimes negative -- set to N/A
stateDat$loessN[which(stateDat$loessN < 0)] <- NA
stateDat <- stateDat %>% ungroup()

rm(df_1,df_2)

######################################################################
# Load maps
######################################################################
states_sf <- get_urbn_map(map = "states", sf = TRUE)

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

######################################################################
# Default Parameters for polotting initial maps (then we update dynamically)
######################################################################
defaultDate1 = as.Date("2020-03-01")
defaultDate2 = as.Date(max(stateDat$date))
defaultState = "Hawaii"
defaultValPlot = "cases"

default_state_counties <- sort((countyDat %>% filter(state==defaultState) %>% distinct(county) )$county ) #4-8-2018
default_state_counties_names <- paste0(defaultState,": ", default_state_counties) #4-8-2018
default_rest_counties <- unique(paste0(countyDat$state,": ", countyDat$county)) #4-8-2018
default_rest_counties_names <- sort(setdiff(default_rest_counties,default_state_counties_names)) #4-8-2018
default_all_label <- paste("All",defaultState)

######################################################################
# Misc Functions
######################################################################
# convert data to percentiles
perc.rank <- function(x) trunc(base::rank(x))/length(x)
colorPallette = colorRamp(colors = c("#ffffff", "#ffbe87", "#e6550d"))
######################################################################

######################################################################
# US Map
######################################################################

# Filter the date range for the data to show on the US map
# Pull in the state data
req(stateDat)
# Get the appropriate date range
usmapDat1 <- subset(stateDat, as.character(date) == as.character(defaultDate1))
usmapDat2 <- subset(stateDat, as.character(date) == as.character(defaultDate2))
# Find the states for that don't have data in the 1st timepoint but do have data in the 2nd timepoint
toAdd = usmapDat2[which(!usmapDat2$state %in% usmapDat1$state),]
if(nrow(toAdd)>0){
  toAdd[,c("cases","deaths")] = 0
  usmapDat1 = rbind(usmapDat1,toAdd)
}
usmapDat2 <- usmapDat2 %>% 
  arrange(state)
usmapDat1 <- usmapDat1 %>% 
  arrange(state)

# Get the difference between the two date ranges (e.g. we want the number of cases or deaths for that date range)
usmapDat2$cases = usmapDat2$cases - usmapDat1$cases
usmapDat2$deaths = usmapDat2$deaths - usmapDat1$deaths

# Scale to percentile
usmapDat2$value = perc.rank(pull(usmapDat2[,defaultValPlot]))
usmapDat2$value = usmapDat2$value*100

# Get the state boundaries
req(states_sf)
usmapDat2 <- states_sf %>%
  left_join(usmapDat2,by = c("state_name" = "state")) %>%
  # transform the coordinates of the map (by default it is very squished)
  st_transform(crs = paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

######################################################################
# Get table to plot
######################################################################

plotTable <- countyDat[which(countyDat$date==max(countyDat$date)),]
plotTable <- plotTable[,c(1,2,3,5,6,7,8,12,13,14,15,19,20,21,22)]
colnames(plotTable) <- c("Date","County","State","Cases","Deaths","New Cases","New Deaths","Cases Per Million","Deaths Per Million",
                         "New Cases Per Million","New Deaths Per Million","County ICU Beds","Predicted ICU Beds Occupied","Predicted Fraction of ICU Beds Occupied",
                         "Doubling Time")

######################################################################
# Save Workspace to file
######################################################################
save.image(file = "./DataFiles/CovidCountiesWorkspace.RData")

# countyDat <- countyDat[which(countyDat$date <= as.Date("2020-04-15")),]
# stateDat <- stateDat[which(stateDat$date <= as.Date("2020-04-15")),]