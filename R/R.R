library(ggplot2)
library(readr)
library(dplyr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(factoextra)
library(usa)
#0. Extract Data and clean some columns. two URLs, for USA and the world.
#urlfile="https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"

#I. USA
urlfile.usa="https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv"
urlfile.govs="https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/us-governors/data/us-governors.csv"


df <- read_csv(url(urlfile.usa)) %>% filter(!is.na(RegionName)) %>%
                                      rename(date = Date) %>%
                                  mutate(date = as.Date(as.character(date), format = '%Y%m%d'))
facts<-facts
governors<-read_csv(url(urlfile.govs))%>%dplyr::select(1:10)

##Merge with US data from USA package. Use library(usa)
df=df%>%left_join(facts,by = c('RegionName'='name'))




#TODO: Exploratory Analysis(some plots about key variables, summary statistics)


#1. Create Datasets
##Define Types of Measures
containment_measures=c('C1_School closing','C2_Workplace closing','C3_Cancel public events','C4_Restrictions on gatherings',
                     'C5_Close public transport','C6_Stay at home requirements','C7_Restrictions on internal movement','C8_International travel controls')
economic_measures=c('E1_Income support','E2_Debt/contract relief','E3_Fiscal measures')
health_measures=c('H1_Public information campaigns','H2_Testing policy','H3_Contact tracing','H4_Emergency investment in healthcare','H5_Investment in vaccines','H6_Facial Coverings',
                  'H8_Protection of elderly people')
country_variables=c('CountryName','CountryCode','RegionName','Date')
outcome_variables=c('ConfirmedDeaths','ConfirmedCases')

##Get the date with the top number of new deaths for each state
df.maxdeaths = df %>% group_by(RegionName) %>% mutate(newdeaths = ConfirmedDeaths -
                                                        lag(ConfirmedDeaths, order_by = date)) %>%
  select(RegionName, date, newdeaths) %>% slice_max(n = 1, order_by = newdeaths) %>%
  arrange(-newdeaths) %>%
  filter(!is.na(RegionName)) %>% rename(date.max = date) %>%
  mutate(date.min30 = date.max - 30, date.plus30 = date.max + 30)

##Subset only numeric variables of interest
df.measures=df[,c("RegionName","date",containment_measures,economic_measures,health_measures)]

##Get the policy measures for 30 days before and after the max date.
df.aroundpeak=df.measures%>%left_join(df.maxdeaths,by = 'RegionName')%>%filter(date>date.min30&date<date.plus30)

##Get mean and max levels of each measure across that timeframe
df.aroundpeak.meanmeasures = df.aroundpeak %>% group_by(RegionName) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  mutate(across(where(is.numeric), round, 3))
df.aroundpeak.maxmeasures = df.aroundpeak %>% group_by(RegionName) %>% summarise_if(is.numeric, max, na.rm = TRUE) %>%
  mutate(across(where(is.numeric), round, 3))
#write.csv(df.aroundpeak.meanmeasures,row.names = F,file="C:\\repos\\covid-measures-uml\\MeasuresUSA.csv")

#Show when each state had its peak and how much was it.
ggplot(df.aroundpeak,aes(x=date,y=newdeaths))+geom_line(aes(color=RegionName))+scale_y_log10()+theme_bw()+ylab('Peak Deaths')

#2. Research question: 
##PCA exploratory analysis
df.pca=df.aroundpeak.meanmeasures%>%select(-'H5_Investment in vaccines',-'newdeaths')
df.pca=df.pca%>%replace_na(list(0))%>%mutate(across(where(is.numeric),~ifelse(is.nan(.x),yes=0,no=.x)))
pca_fit <- select(df.pca[,], -RegionName) %>%
  scale() %>% 
  prcomp(); summary(pca_fit)

pca_out=get_pca(pca_fit)
df.coords.pc=pca_out$coord%>%as.data.frame()
df.coords.pc=df.coords.pc%>%mutate(varnames=rownames(df.coords.pc))
#df.coords.pc=pca_fit$rotation%>%as.data.frame()
df.coords.x=pca_fit$x%>%as.data.frame()%>%mutate(RegionName=df.pca$RegionName,maxdeaths=df.aroundpeak.meanmeasures$newdeaths)

###Plot without color
ggplot(df.coords.pc,aes(x=Dim.1,y=3*Dim.2))+geom_segment(aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")+
  geom_text( aes(x=5*Dim.1, y=5*Dim.2, label=varnames), size = 4,alpha=0.75, vjust=1, color="red")+
  geom_point(data=df.coords.x, aes(x=PC1 , y=PC2 ), size = 1,alpha=0.75, color="blue")+
  geom_text(data=df.coords.x, aes(x=PC1, y=PC2, label=RegionName), size = 3,alpha=0.75, vjust=1, color="black")+
  theme_bw()

###Color by maximum number of deaths. TODO: scale by population in the future.
ggplot(df.coords.x)+geom_segment(data=df.coords.pc,aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")+
  geom_text(data=df.coords.pc, aes(x=5*Dim.1, y=5*Dim.2, label=varnames), size = 4,alpha=0.75, vjust=1, color="red")+
  geom_point(data=df.coords.x, aes(x=PC1 , y=PC2,color=log(maxdeaths) ), size = 5,alpha=0.75)+
  geom_text(data=df.coords.x, aes(x=PC1, y=PC2, label=RegionName), size = 3,alpha=0.75, vjust=1, color="black")+
theme_bw()+scale_color_continuous(type = 'viridis')+labs(color='Log Max Daily Deaths')

#TODO: similar plots but considering maximum variables
#TODO: color by political, urban/non-urban, GDP variables. 
#TODO: find countries most similar to each U.S. State(could be only from Europe). Color by absolute difference. 

#3. Start Analysis with SOM

data(stat)