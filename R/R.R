library(ggplot2)
library(readr)
library(dplyr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(factoextra)
library(usa)
library(amerika)
library(ggrepel)
library(cowplot)
library(mixtools)
library(clValid)
library(mclust)
library(poliscidata)
library(kableExtra)
setwd('C:\\repos\\covid-measures-uml\\')


point_colors <- c(amerika_palettes$Republican[2], 
                  amerika_palettes$Democrat[2])

#0. Extract Data and clean some columns. two URLs, for USA and the world.
#urlfile="https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"

#0. Import and clean
urlfile.usa="https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv"
urlfile.govs="https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/us-governors/data/us-governors.csv"

df <- read_csv(url(urlfile.usa)) %>% filter(!is.na(RegionName)) %>%
                                      rename(date = Date) %>%
                                  mutate(date = as.Date(as.character(date), format = '%Y%m%d'))
facts<-facts%>%select(name,population)
governors<-read_csv(url(urlfile.govs))%>%dplyr::select(1:7)
state_variables=facts%>%left_join(governors,by = c('name'='state_name'))%>%rename('RegionName'='name')
state_poliscidata=poliscidata::states%>%select(state,stateid,conpct_m,conserv_advantage,dempct_m)%>%mutate(state=trimws(as.character(state)),isRep=conpct_m>dempct_m,advantage=conpct_m-dempct_m)%>%as.data.frame()

state_variables=state_variables%>%left_join(state_poliscidata,by=c('RegionName'='state'))%>%
              mutate(coincides=ifelse((party=='republican'&isRep)|(party=='democrat'&!isRep),1,0),
                                                                   coincidesRobust=ifelse((party=='republican'&advantage<(-5))|(party=='democrat'&advantage>5),yes = 0,1))%>%mutate(leaning=ifelse(coincides==1,yes=party,no=ifelse(party=='republican',yes='democrat',no='republican')))
outliers=state_variables%>%filter(coincidesRobust==0)%>%select(RegionName,party,conpct_m,dempct_m,isRep,advantage,coincides,coincidesRobust)

##Merge with US data from USA package. Use library(usa)
df=df%>%left_join(facts,by = c('RegionName'='name'))%>%mutate(deaths_1000=ConfirmedDeaths/(population*1000),cases_1000=ConfirmedCases/(population*1000))
df=df%>%left_join(governors,by = c('RegionName'='state_name'))

#1. Create Datasets
##Define Types of Measures
containment_measures=c('C1_School closing','C2_Workplace closing','C3_Cancel public events','C4_Restrictions on gatherings',
                     'C5_Close public transport','C6_Stay at home requirements','C7_Restrictions on internal movement',
                     'C8_International travel controls')
economic_measures=c('E1_Income support','E2_Debt/contract relief','E3_Fiscal measures')

health_measures=c('H1_Public information campaigns','H2_Testing policy','H3_Contact tracing','H4_Emergency investment in healthcare',
                  'H5_Investment in vaccines','H6_Facial Coverings',
                  'H8_Protection of elderly people')
country_variables=c('CountryName','CountryCode','RegionName','Date')
outcome_variables=c('ConfirmedDeaths','ConfirmedCases')

df.measures=df[,c("RegionName","date",containment_measures,economic_measures,health_measures)]

##Get the date with the top number of new deaths for each state
df.maxdeaths = df %>% group_by(RegionName) %>% mutate(newdeaths = ConfirmedDeaths -
                                                        lag(ConfirmedDeaths, order_by = date)) %>%
  select(RegionName, date, newdeaths) %>% slice_max(n = 1, order_by = newdeaths) %>%
  arrange(-newdeaths) %>%
  filter(!is.na(RegionName)) %>% rename(date.max = date) %>%
  mutate(date.min30 = date.max - 30, date.plus30 = date.max + 30)

##Get the policy measures for 30 days before and after the max date.
df.aroundpeak=df.measures%>%left_join(df.maxdeaths,by = 'RegionName')%>%filter(date>date.min30&date<date.plus30)

##Get mean and max levels of each measure across that timeframe
df.aroundpeak.meanmeasures = df.aroundpeak %>% group_by(RegionName) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  mutate(across(where(is.numeric), round, 3))
df.aroundpeak.maxmeasures = df.aroundpeak %>% group_by(RegionName) %>% summarise_if(is.numeric, max, na.rm = TRUE) %>%
  mutate(across(where(is.numeric), round, 3))
#save(df.aroundpeak.meanmeasures,row.names = F,file="C:\\repos\\covid-measures-uml\\MeasuresUSA.Rdata")
##write.csv(df.aroundpeak.meanmeasures,row.names = F,file="C:\\repos\\covid-measures-uml\\MeasuresUSA.csv")

##Show when each state had its peak and how much was it.
ggplot(df.aroundpeak,aes(x=date,y=newdeaths))+geom_line(aes(color=RegionName))+scale_y_log10()+theme_bw()+ylab('Peak Deaths')

##Descriptive analysis: TODO Scale variables by maximum value in total(need auxiliary dataset)
measures.vector=c(containment_measures,economic_measures,health_measures)#%>%select(`E3_Fiscal measures`, `H4_Emergency investment in healthcare`)%>%
measures.summary=df.aroundpeak%>%select(all_of(measures.vector))%>%
                  pivot_longer(cols=(1:length(measures.vector)))%>%group_by(name)%>%
                  summarise(N=length(name),mean=mean(value,na.rm = T),sd=sd(value,na.rm = T),min=min(value,na.rm = T),max=max(value,na.rm = T))%>%filter(mean<10&mean>0)

measures.summary.mean=rbind(top5.mean,bottom5.mean)
measures.summary.simple=measures.summary%>%mutate(type=substr(name,1,1),name_clean=substr(name,4,nchar(name)))
measure_types=measures.summary.simple%>%select(name,name_clean,type)%>%rbind(data.frame(name=c('E3_Fiscal measures','H4_Emergency investment in healthcare'),name_clean=c('Fiscal measures','Emergency investment in healthcare'),type=c('E','H')))
ggplot(measures.summary.simple,aes(x=mean,y=sd))+geom_point(aes(color=type))+geom_text_repel(aes(label=name))+theme_bw()

save(df.aroundpeak.meanmeasures,file = '.\\data\\MeasuresAroundPeakDf.Rdata')
