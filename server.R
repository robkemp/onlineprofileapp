library(shiny)
library(stringr)
library(codemog)
library(codemogProfile)
library(dplyr)
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/county_est.rdata")
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/county_forecast.rdata")
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/muni_est.rdata")
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/muni_win_est.rdata")
load("/opt/shiny-server/samples/sample-apps/onlineprofileapp/data/muni_hist.rdata")


county_forecast=county_forecast
muni_est=muni_est
county_est=county_est
muni_hist=muni_hist
muni_win_est=muni_win_est

c_names=county_est%>%
  filter(year==2010)%>%
  select(countyfips, county)%>%
  mutate(countyfips=str_sub(paste("000",countyfips, sep=""), -3,-1))
m_names=muni_est%>%
  filter(year==2010)%>%
  select(placefips, municipality)%>%
  mutate(placefips=str_sub(paste("00000",placefips, sep=""), -5,-1))

county_ts_chart=function(fips, beginyear=1990, endyear=2013, base=12){
  require(dplyr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)  
  require(grid, quietly=TRUE)
  fips=as.numeric(fips)
  
  d=county_forecast%>%
    filter(countyfips==fips, year<=endyear, year>=beginyear)%>%
    group_by(county,countyfips, year)%>%
    summarise(totalPopulation=sum(totalPopulation))%>%
    mutate(type=ifelse(year>=2014, "Forecast", "Estimate"))
  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=countyfips))+
    geom_line(color=codemog_pal['dkblu'], size=1.75)+
    labs(x="Year", y="Population", title=paste(d$county,"County Population,", beginyear, "to", endyear, sep=" "))+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
  theme(axis.text.x=element_text(angle=90))
  return(p)
}

muni_ts_chart=function(fips, beginyear=1990, endyear=2013, base=12){
  require(dplyr, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)  
  require(grid, quietly=TRUE)
  fips=as.numeric(fips)
  
  d=muni_est%>%
    mutate(year=as.numeric(as.character(year)),
           placefips=as.numeric(as.character(placefips)),
           geonum=as.numeric(as.character(geonum)))%>%
    select(geonum, placefips, municipality, year, totalPopulation)%>%
    bind_rows(muni_hist%>%select(-countyfips))%>%
    filter(year>=beginyear, year<=endyear)%>%
    group_by(placefips,municipality,year)%>%
    summarise(totalPopulation=sum(totalPopulation))%>%
    filter(placefips==fips)
  
  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=placefips))+
    geom_line(color=codemog_pal['dkblu'], size=1.75)+
    labs(x="Year", y="Population", title=paste(d$municipality,"Population,", beginyear, "to", endyear, sep=" "))+
    scale_y_continuous(label=comma)+
    theme_codemog(base_size=base)+
    theme(axis.text.x=element_text(angle=90))
  return(p)
}

shinyServer(function(input, output) {
####County Stuff ####
  cntyfips=reactive({c_names%>%filter(county==input$cnty)%>%select(countyfips)})
  #Population Chart
  cpop_plot_input=reactive({county_ts_chart(as.numeric(cntyfips()))})
  output$cpop_plot=renderPlot({cpop_plot_input()})
 #Population Table
#cmuniwin_input=reactive({muni_win_est%>%filter(year==2013, countyfips==cntyfips()})
#output$cmuniwin_table=renderDataTable({cmuniwin_input()})
 # Population Forecast Chart
 cpop_fplot_input=reactive({county_ts_chart(as.numeric(cntyfips()), 2014,2040)})
 output$cpop_fplot=renderPlot({cpop_fplot_input()})
 # Household Chart
 chh_plot_input=reactive({ms_hh(cntyfips())})
 output$chh_plot=renderPlot({chh_plot_input()})
 # Race 
 crace_table_input=reactive({ms_race(cntyfips())%>%select(race, Census.2010, Census.2000)})
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