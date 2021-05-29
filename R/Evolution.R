#Define times of analysis
around=c(-60,-45,-15,0,15,30,45,60)
around=c(-60,0,60)
around=c(0)

#Create dataframe of measures at different points
df.meanmeasures.evolution=around%>%map_dfr(function(x)  df.measures %>% left_join(df.maxdeaths, by = 'RegionName') %>%
  filter(date > (date.min30+x) & date < (date.plus30+x))%>%mutate(reference=x)%>% group_by(RegionName,reference) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>%
    mutate(across(where(is.numeric), round, 3)))

evolution.measures=around%>%map_dfr(function(x) (df.meanmeasures.evolution%>%select(-'H5_Investment in vaccines',-'newdeaths')%>%filter(reference==x)%>% 
                                                   mutate(across(where(is.numeric),~ifelse(is.nan(.x),yes=0,no=.x)))%>%as.data.frame()%>%select(-RegionName,-reference)%>%scale( center=pca_fit$center,scale=pca_fit$scale)) %*% pca_fit$rotation%>%
                                      as.data.frame()%>%select(PC1,PC2)%>%mutate(reference=x,RegionName=df.meanmeasures.evolution$RegionName[df.meanmeasures.evolution$reference==x]))
rownames(new.coordinates)
estados=sample(unique(evolution.measures$RegionName),size = 49)
estados=c('Utah','North Dakota','Maine','South Carolina','Vermont','Pennsylvania','New York','Maryland')

full_evolution=ggplot(evolution.measures,aes(x=PC1,y=PC2,color=(reference)))+
  geom_point()+geom_path(aes(group=RegionName,color=reference))+
  scale_color_gradient(low="blue", high="red")+facet_wrap(~RegionName,ncol = 7,nrow = 8)

selected.states=evolution.measures%>%filter(RegionName%in%estados)
most_variance_category=c('E2_Debt/contract relief','H6_Facial Coverings','C7_Restrictions on internal movement')
df.coords.pc.2=df.coords.pc%>%filter(varnames%in%most_variance_category)

selected_evolution=ggplot(data=selected.states,aes(x=PC1,y=PC2,color=(reference)))+
  geom_segment(data=df.coords.pc.2,mapping =aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=1, color="black",inherit.aes = FALSE)+
  geom_label_repel(data=df.coords.pc.2, mapping =aes(x=15*Dim.1, y=15*Dim.2, label=substr(name_clean,1,27)), size = 2,alpha=1, vjust=1,inherit.aes = FALSE)+
  geom_point(data=selected.states)+geom_path(data=selected.states,mapping=aes(group=RegionName,color=reference))+
  scale_color_gradientn(colours=c("blue","red","green"))+facet_wrap(~RegionName,ncol = 2,nrow = 4)
  labs(color='Days since peak')+theme(legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))+guides(fill = guide_legend(label.position = "bottom"))


