library(shiny)
library(codemog)
library(dplyr)
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/county_est.rdata")
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/county_forecast.rdata")
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/muni_est.rdata")

c_names=county_est%>%
  filter(year==2010)%>%
  select(countyfips, county)
m_names=muni_est%>%
  filter(year==2010)%>%
  select(placefips, municipality)
# Define UI for slider demo application
shinyUI(fluidPage(
  navbarPage( "Colorado Demographic and Economic Profile",
              tabPanel("County",
  pageWithSidebar( 
    headerPanel("County Profile"),
       sidebarPanel(
          selectInput("cnty","County:",
                      choices=c_names$county)),
       mainPanel(
         tabsetPanel(
              tabPanel("Population",
                       plotOutput("cpop_plot"),
                       downloadButton('downloadpopData', "Download Data"),
                       dataTableOutput('pop_table')),
              tabPanel("Population Forecast",
                       plotOutput("cpop_fplot"),
                       downloadButton('downloadfpopData', "Download Data"),
                       dataTableOutput('fpop_table'))),
              tabPanel("Housing & Households",
                       plotOutput("chh_plot")),
              tabPanel("Race & Ethnicity",
                       dataTableOutput("crace_table")),
              tabPanel("Income & Education",
                       plotOutput("cinc_plot"),
                       plotOutput("ced_plot")), 
              tabPanel("Jobs & the Economy"))
    
    ))),
  tabPanel("Municipality",
           pageWithSidebar( 
             headerPanel("Municipal Profile"),
             sidebarPanel(
               selectInput("muni","Municipality:",
                           choices=m_names$municipality)),
             mainPanel(
               tabsetPanel(
                 tabPanel("Population",
                          plotOutput("mpop_plot")),
                 tabPanel("Housing & Households",
                          plotOutput("mhh_plot")),
                 tabPanel("Race & Ethnicity",
                          dataTableOutput("mrace_table")),
                 tabPanel("Income & Education",
                          plotOutput("minc_plot"),
                          plotOutput("med_plot")), 
                 tabPanel("Jobs & the Economy"))
               
             )))
)))