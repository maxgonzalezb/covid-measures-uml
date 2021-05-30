library(maps)    
map_states <- map_data("state")%>%mutate(stateabbr=substr(region,1,2))
clustering.results.map=clustering.results%>%mutate(region=tolower(RegionName))%>%left_join(map_states)

centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
centroids$abb<-state.abb[match(centroids$region,tolower(state.name))]
centroids=centroids%>%filter(!(region%in%c('alaska','hawaii','puerto rico')))

p.map=ggplot(clustering.results.map%>%filter(!(region%in%c('alaska','hawaii')))) +geom_polygon( aes(x=long, y=lat, group=group,fill=cluster),
                color="black")+geom_text(data=centroids,aes(x = long, y=lat, label = abb), 
                                             size = 2,color="white")+theme_g

png(filename="C:\\repos\\covid-measures-uml\\Presentation\\presentation-figures\\map_clusters.png",width = 14, height = 7,
    units = "in",res=1000)
p.map
dev.off()
                
theme_g<-
  theme(panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border =  element_blank(),
        plot.title = element_text(
          size = 15, hjust = 0.5,colour = "white"),
        legend.title= element_text(
          hjust = 0.4 ,vjust=0.3, size=10)#,
        #legend.text = element_text(
          #hjust = 0.4 ,vjust=2, size=8)
  )
