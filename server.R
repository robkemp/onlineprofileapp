library(shiny)
library(stringr)
library(codemog)
library(codemogProfile)
library(dplyr)
load("data/County_Est")
load("data/County_Forecast")
load("data/muni_est")


c_names=county_est%>%
  filter(year==2010)%>%
  select(countyfips, county)%>%
  mutate(countyfips=str_sub(paste("000",countyfips, sep=""), -3,-1))
m_names=muni_est%>%
  filter(year==2010)%>%
  select(placefips, municipality)%>%
  mutate(placefips=str_sub(paste("00000",placefips, sep=""), -5,-1))

shinyServer(function(input, output) {
####County Stuff ####
  cntyfips=reactive({c_names%>%filter(county==input$cnty)%>%select(countyfips)})
  #Population Chart
  cpop_plot_input=reactive({county_ts_chart(as.numeric(cntyfips()))})
  output$cpop_plot=renderPlot({cpop_plot_input()})
 
 # Population Forecast Chart
 cpop_fplot_input=reactive({county_ts_chart(as.numeric(cntyfips()), 2014,2040)})
 output$cpop_fplot=renderPlot({cpop_fplot_input()})
 # Household Chart
 chh_plot_input=reactive({ms_hh(cntyfips())})
 output$chh_plot=renderPlot({chh_plot_input()})
 # Race Chart
 crace_table_input=reactive({ms_race(cntyfips())%>%select(race, Census.2010, Census.2000, Change)})
 output$crace_table=renderDataTable({crace_table_input()})
 # Income Chart
  cinc_plot_input=reactive({ms_income(cntyfips())})
  output$cinc_plot=renderPlot({cinc_plot_input()})
 # Education Chart
  ced_plot_input=reactive({ms_ed(cntyfips())})
  output$ced_plot=renderPlot({ced_plot_input()})


####Muni Stuff ####
munifips=reactive({m_names%>%filter(municipality==input$muni)%>%select(placefips)})
#Population Chart
mpop_plot_input=reactive({muni_ts_chart(as.numeric(munifips()))})
output$mpop_plot=renderPlot({mpop_plot_input()})
# Household Chart
mhh_plot_input=reactive({ms_hh(munifips())})
output$mhh_plot=renderPlot({mhh_plot_input()})
# Race Chart
mrace_table_input=reactive({ms_race(munifips())%>%select(race, Census.2010, Census.2000, Change)})
output$mrace_table=renderDataTable({mrace_table_input()})
# Income Chart
minc_plot_input=reactive({ms_income(munifips())})
output$minc_plot=renderPlot({minc_plot_input()})
# Education Chart
med_plot_input=reactive({ms_ed(munifips())})
output$med_plot=renderPlot({med_plot_input()})
})