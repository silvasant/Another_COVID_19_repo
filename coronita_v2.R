library(tidyverse)
library(tseries)
library(xts)
library(dygraphs)
library(shiny)
library(leaflet)
library(lubridate)

coronavirus_dataset<-read.csv('https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv')

SelectedCountry<-'Argentina'
coronavirus_dataset %>% filter(Country.Region==SelectedCountry,type=='confirmed') %>% select(date,type,cases) %>%  group_by(date,type)  %>% summarise('new_confirmed_cases'=sum(cases))  %>% ungroup()  %>% mutate('cummulative_confirmed'=cumsum(new_confirmed_cases)) %>% select(-type) ->COVID_CONF
coronavirus_dataset %>% filter(Country.Region==SelectedCountry,type=='recovered') %>% select(date,type,cases) %>%  group_by(date,type)  %>% summarise('new_recovered_Cases'=sum(cases))  %>% ungroup()  %>% mutate('cummulative_recovered'=cumsum(new_recovered_Cases))%>% select(-type)  ->COVID_RECOVERED
coronavirus_dataset %>% filter(Country.Region==SelectedCountry,type=='death') %>% select(date,type,cases) %>%  group_by(date,type)  %>% summarise('new_deceased_cases'=sum(cases))  %>% ungroup()  %>% mutate('cummulative_deceased'=cumsum(new_deceased_cases)) %>% select(-type) ->COVID_DECEASED
COVID_AGG_DATA <-
  COVID_CONF %>% left_join(COVID_DECEASED, 'date') %>% left_join(COVID_RECOVERED,'date')
COVID_AGG_DATA$cummulative_active_actual <-
  COVID_AGG_DATA$cummulative_confirmed  - COVID_AGG_DATA$cummulative_recovered -
  COVID_AGG_DATA$cummulative_deceased
COVID_AGG_DATA$mortality <-
  ifelse(
    COVID_AGG_DATA$cummulative_confirmed == 0,
    0,
    COVID_AGG_DATA$cummulative_deceased / COVID_AGG_DATA$cummulative_confirmed
  )
COVID_AGG_DATA$date<-ymd(COVID_AGG_DATA$date)
xts( COVID_AGG_DATA[,-1],order.by = COVID_AGG_DATA$date ) %>% dygraph(main = sprintf('COVID-19 cases: %s',SelectedCountry)) 

