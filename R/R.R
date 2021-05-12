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
  
#2. Research question: 
##2.1 PCA exploratory analysis
df.pca=df.aroundpeak.meanmeasures%>%select(-'H5_Investment in vaccines',-'newdeaths')
df.pca=df.pca%>%replace_na(list(0))%>%mutate(across(where(is.numeric),~ifelse(is.nan(.x),yes=0,no=.x)))
df.pca=df.pca%>%replace_na(list(0))%>%mutate(across(where(is.numeric),scale))
pca_fit <- select(df.pca[,], -RegionName) %>%
  scale() %>% 
  prcomp(); summary(pca_fit)

pca_out=get_pca(pca_fit)
df.coords.pc=pca_out$coord%>%as.data.frame()
df.coords.pc=df.coords.pc%>%mutate(varnames=rownames(df.coords.pc))%>%left_join(measure_types,by=c('varnames'='name'))

#Create a datset with the results and other variables
df.coords.x=pca_fit$x%>%as.data.frame()%>%mutate(RegionName=df.pca$RegionName,maxdeaths=df.aroundpeak.meanmeasures$newdeaths)%>%left_join(state_variables)%>%mutate(deaths_1000=(maxdeaths*1000)/(population))

##Plots
p.dims.onlydims=ggplot(df.coords.pc,aes(x=Dim.1,y=3*Dim.2))+geom_segment(aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="black")+
  geom_label_repel( aes(x=5*Dim.1, y=5*Dim.2, label=substr(name_clean,1,27),color=type), size = 3.5,alpha=1, vjust=1,fontface  = "bold")+
  geom_point(data=df.coords.x, aes(x=PC1 , y=PC2 ), size = 1.3,alpha=0.9, color="lightblue")+
  theme_bw()+xlab('')+ylab('')+xlim(-5.5,6.5)+theme(legend.position = 'none')
p.dims.onlydims

p.dims.states=ggplot(df.coords.pc,aes(x=Dim.1,y=3*Dim.2))+geom_segment(aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.25, color="black")+
  geom_label_repel( aes(x=5*Dim.1, y=5*Dim.2, label=substr(name_clean,1,27),color=type), size = 2.5,alpha=1, vjust=1)+
  #geom_point(data=df.coords.x, aes(x=PC1 , y=PC2 ), size = 1.3,alpha=0.9, color="lightblue")+
  geom_text(data=df.coords.x, aes(x=PC1, y=PC2, label=RegionName), size = 3.5,alpha=0.75, vjust=1, color="black")+
  theme_bw()+xlab('')+ylab('')+xlim(-5.5,6.5)+theme(legend.position = 'none')
p.dims.states

## 2.2 Clustering
df.clustering=df.pca%>%as.data.frame()
df.clustering=df.aroundpeak.meanmeasures%>%select(-'H5_Investment in vaccines',-'newdeaths',-`H4_Emergency investment in healthcare`,-`H1_Public information campaigns`)%>%
  replace_na(list(0))%>%mutate(`E3_Fiscal measures`= ifelse(is.nan(`E3_Fiscal measures`),yes=0,no=`E3_Fiscal measures`))%>%
  select(-'E3_Fiscal measures')%>%as.data.frame()%>%mutate(across(where(is.numeric),scale))%>%as.data.frame()
df.clustering= df.clustering[,-1]
tidy.name.vector <- make.names(colnames(df.clustering), unique=TRUE)
colnames(df.clustering)<-tidy.name.vector

set.seed(3)
nclusts=4
kmeans.cluster=kmeans(df.clustering,centers = nclusts,iter.max = 1000)
clustering.results=(df.coords.x)%>%mutate(cluster=kmeans.cluster$cluster)
p.cluster=ggplot(df.coords.pc,aes(x=Dim.1,y=3*Dim.2))+geom_segment(aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.25, color="black")+
  #geom_label_repel( aes(x=5*Dim.1, y=5*Dim.2, label=substr(name_clean,1,27)), size = 2.5,alpha=1, vjust=1)+
  #geom_point(data=df.coords.x, aes(x=PC1 , y=PC2 ), size = 1.3,alpha=0.9, color="lightblue")+
  geom_label(data=clustering.results, aes(x=PC1, y=PC2, label=RegionName,color=as.factor(cluster)), size = 3.5,alpha=0.75, vjust=1)+
  theme_bw()+xlab('')+ylab('')+xlim(-5.5,6.5)+theme(legend.position = 'none')
  p.cluster

##2.3 COlor by segmenting variables
## Color by maximum number of deaths. 
ggplot(df.coords.x)+geom_segment(data=df.coords.pc,aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")+
  geom_text(data=df.coords.pc, aes(x=5*Dim.1, y=5*Dim.2, label=varnames), size = 4,alpha=0.75, vjust=1, color="red")+
  geom_point(data=df.coords.x, aes(x=PC1 , y=PC2,color=log(deaths_1000) ), size = 5,alpha=0.75)+
  geom_text(data=df.coords.x, aes(x=PC1, y=PC2, label=RegionName), size = 3,alpha=0.75, vjust=1, color="black")+
theme_bw()+scale_color_continuous(type = 'viridis')+labs(color='Log Max Daily Deaths')

###Color by partisanship
plot.partisan.governors.varnames<-ggplot(df.coords.x)+geom_segment(data=df.coords.pc,aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="darkgrey",lwd=.8)+
  geom_text_repel(data=df.coords.pc, aes(x=5*Dim.1, y=5*Dim.2, label=name_clean), size = 4,alpha=0.75, vjust=1, color="black")+
  #geom_poi|nt(data=df.coords.x, aes(x=PC1 , y=PC2,color= party), size = 3,alpha=0.55)+
  geom_text(data=df.coords.x, aes(x=PC1, y=PC2, label=RegionName,color=party), size = 3.5,alpha=1, vjust=1.5)+
  theme_bw()+scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                         amerika_palettes$Democrat[1]),
                                name="Party",
                                breaks=c("republican", "democrat"),
                                labels=c("Republican", "Democrat")) +theme(legend.position = 'none')+xlim(-5.5,6.5)#+scale_color_continuous(type = 'viridis')+labs(color='Log Max Daily Deaths')

plot.partisan.governors.novarnames<-ggplot(df.coords.x)+geom_segment(data=df.coords.pc,aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="darkgrey",lwd=.8)+
  #geom_text_repel(data=df.coords.pc, aes(x=5*Dim.1, y=5*Dim.2, label=name_clean), size = 4,alpha=0.75, vjust=1, color="black")+
  #geom_point(data=df.coords.x, aes(x=PC1 , y=PC2,color= party), size = 3,alpha=0.55)+
  geom_text(data=df.coords.x, aes(x=PC1, y=PC2, label=RegionName,color=party), size = 3.5,alpha=1, vjust=1.5)+
  theme_bw()+scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                         amerika_palettes$Democrat[1]),
                                name="Party",
                                breaks=c("republican", "democrat"),
                                labels=c("Republican", "Democrat")) +theme(legend.position = 'none')+xlim(-5.5,6.5)#+scale_color_continuous(type = 'viridis')+labs(color='Log Max Daily Deaths')

##Study if alternative classification is better
plot.partisan.leanings<-ggplot(df.coords.x)+geom_segment(data=df.coords.pc,aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="darkgrey",lwd=.8)+
  #geom_text_repel(data=df.coords.pc, aes(x=5*Dim.1, y=5*Dim.2, label=name_clean), size = 4,alpha=0.75, vjust=1, color="black")+
  #geom_point(data=df.coords.x, aes(x=PC1 , y=PC2,color= party), size = 3,alpha=0.55)+
  geom_text(data=df.coords.x, aes(x=PC1, y=PC2, label=RegionName,color=leaning,alpha=(1-coincides)), size = 3.5, vjust=1.5)+
  theme_bw()+scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                         amerika_palettes$Democrat[1]),
                                name="Party",
                                breaks=c("republican", "democrat"),
                                labels=c("Republican", "Democrat")) +theme(legend.position = 'none')+xlim(-5.5,6.5)#+scale_color_continuous(type = 'viridis')+labs(color='Log Max Daily Deaths')

##Create tables for output
outliers.table=outliers%>%select(RegionName,party,advantage,leaning)%>%rename('State'='RegionName','Party'='party','Republican Advantage in Population'='advantage','Leaning'='leaning')%>%kable(format = 'latex',booktabs = T,digits = 2,
                                                                                                                                                                                                 linesep = "",
                                                                                                                                                                                                 align = rep('c', 4))

##Create Plots for output
png(filename="C:\\repos\\covid-measures-uml\\Presentation\\presentation-figures\\plot_partisan_governors_varnames.png",width = 12, height = 7,
    units = "in",res=1000)
plot.partisan.governors.varnames
dev.off()

png(filename="C:\\repos\\covid-measures-uml\\Presentation\\presentation-figures\\plot_partisan_governors_novarnames.png",width = 12, height = 7,
    units = "in",res=1000)
plot.partisan.governors.novarnames
dev.off()

png(filename="C:\\repos\\covid-measures-uml\\Presentation\\presentation-figures\\plot_partisan_leanings.png",width = 12, height = 7,
    units = "in",res=1000)
plot.partisan.leanings
dev.off()

png(filename="C:\\repos\\covid-measures-uml\\Presentation\\presentation-figures\\dimension_space.png",width = 12, height = 7,
    units = "in",res=1000)
p.dims.onlydims
dev.off()

png(filename="C:\\repos\\covid-measures-uml\\Presentation\\presentation-figures\\dimension_space_states.png",width = 12, height = 7,
    units = "in",res=1000)
p.dims.states
dev.off()

png(filename="C:\\repos\\covid-measures-uml\\Presentation\\presentation-figures\\clusters.png",width = 12, height = 7,
    units = "in",res=1000)
p.cluster
dev.off()

##Clustering similar types of responses: had

#NOT RUN
{
#st_prof.internal.gmm <- clValid (df.clustering[,-1]%>%as.matrix(),
 #                                     clMethods = c('hierarchical',"kmeans", "model"),
  #                                    validation = "internal",nClust = 2:10)
# nclusts=4
# nvars=ncol(df.clustering)
# kmeans.cluster=kmeans(df.clustering,centers = nclusts)
# cents=  split(kmeans.cluster$centers, seq(nrow(kmeans.cluster$centers)))
# sigma2=rep(list(0.5*diag(nvars)),nclusts)
# gmm1 <- mvnormalmixEM(x = df.clustering[,],k = 3,verb = T,epsilon = 0.15,mu = cents)
}
OLD{
  # top5.mean=measures.summary%>%slice_max(order_by = mean,n=5)%>%mutate(Type='Top 5, Mean')
  # bottom5.mean=measures.summary%>%slice_min(order_by = mean,n=5)%>%mutate(Type='Bootom 5, Mean')
  # top5.sd=measures.summary%>%slice_max(order_by = sd,n=5)%>%mutate(Type='Top 5, SD')
  # bottom5.sd=measures.summary%>%slice_min(order_by = sd,n=5)%>%mutate(Type='Bootom 5, SD')
  # 
  
#   p1<-ggplot(top5.mean,aes(x=reorder(name, mean),y=mean))+geom_bar(stat='identity',color='black',fill='deepskyblue4')+coord_flip()+facet_wrap(~Type)+theme_bw()+ylab('Mean')+xlab('')+theme(strip.text  =  element_text(size=15),axis.text = element_text(size=12,color='black'))
#   p2<-ggplot(bottom5.mean,aes(x=reorder(name, -mean),y=mean))+geom_bar(stat='identity',color='black',fill='deepskyblue4')+coord_flip()+facet_wrap(~Type)+theme_bw()+ylab('Mean')+xlab('')+theme(strip.text  =  element_text(size=15),axis.text = element_text(size=12,color='black'))
#   p3<-ggplot(top5.sd,aes(x=reorder(name, sd),y=mean))+geom_bar(stat='identity',color='black',fill='deepskyblue4')+coord_flip()+facet_wrap(~Type)+theme_bw()+ylab('Mean')+xlab('')+theme(strip.text  =  element_text(size=15),axis.text = element_text(size=12,color='black'))
#   p4<-ggplot(bottom5.sd,aes(x=reorder(name, -sd),y=mean))+geom_bar(stat='identity',color='black',fill='deepskyblue4')+coord_flip()+facet_wrap(~Type)+theme_bw()+ylab('Mean')+xlab('')+theme(strip.text  =  element_text(size=15),axis.text = element_text(size=12,color='black'))
#   cplot=cowplot::plot_grid(plotlist = list(p1,p2),ncol = 2,nrow = 2)
 }