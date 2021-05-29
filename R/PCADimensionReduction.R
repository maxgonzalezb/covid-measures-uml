set.seed(3)
#Start with df
##2.1 PCA exploratory analysis
df.pca=df.aroundpeak.meanmeasures%>%select(-'H5_Investment in vaccines',-'newdeaths')
df.pca=df.pca%>%replace_na(list(0))%>%mutate(across(where(is.numeric),~ifelse(is.nan(.x),yes=0,no=.x)))
df.pca=df.pca%>%replace_na(list(0))%>%mutate(across(where(is.numeric),scale))
pca_fit <- select(df.pca[,], -RegionName) %>%
  prcomp(); summary(pca_fit)

pca_out=get_pca(pca_fit)
df.coords.pc=pca_out$coord%>%as.data.frame()
df.coords.pc=df.coords.pc%>%mutate(varnames=rownames(df.coords.pc))%>%left_join(measure_types,by=c('varnames'='name'))

#Create a datset with the results and other variables
df.coords.x=pca_fit$x%>%as.data.frame()%>%mutate(RegionName=df.pca$RegionName,maxdeaths=df.aroundpeak.meanmeasures$newdeaths)%>%left_join(state_variables)%>%mutate(deaths_1000=(maxdeaths*1000)/(population))
fviz_pca_biplot(pca_fit, label = "var")
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

#Study Different Types of Clustering
clustering.matrix=df.clustering[,-1]%>%as.matrix()
rownames(clustering.matrix)=df.clustering[,1]
clustering.analysis <- clValid (clustering.matrix,
                                     clMethods = c('hierarchical',"kmeans", "model"),
                                   validation = "internal",nClust = 2:10)
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(clustering.analysis, legend=FALSE)
plot(nClusters(clustering.analysis),measures(clustering.analysis,"Dunn")[,,1],type="n",axes=F,xlab="",ylab="")
legend("center", clusterMethods(clustering.analysis), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

#The most reasonable seems to be k-means at 4, combining metrics with domain expertise.
nclusts=4
kmeans.cluster=kmeans(df.clustering,centers = nclusts,iter.max = 1000,nstart=10)

clustering.results=(df.coords.x)%>%mutate(cluster=kmeans.cluster$cluster)
p.cluster=ggplot(df.coords.pc,aes(x=Dim.1,y=3*Dim.2))+geom_segment(aes(x=0, y=0, xend=5*Dim.1, yend=5*Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.25, color="black")+
  #geom_label_repel( aes(x=5*Dim.1, y=5*Dim.2, label=substr(name_clean,1,27)), size = 2.5,alpha=1, vjust=1)+
  #geom_point(data=df.coords.x, aes(x=PC1 , y=PC2 ), size = 1.3,alpha=0.9, color="lightblue")+
  geom_label_repel(data=clustering.results, aes(x=PC1, y=PC2, label=RegionName,color=as.factor(cluster)), size = 3.5,alpha=0.75, vjust=1)+
  theme_bw()+xlab('')+ylab('')+xlim(-5.5,6.5)+theme(legend.position = 'none')
p.cluster

#Interpret the clusters by showing specific variables in them 
#Select three variables, get min, max, average by cluster. 
most_variance_category=c('E2_Debt/contract relief','H6_Facial Coverings','C7_Restrictions on internal movement')
clustering.results.interpret=df.aroundpeak.meanmeasures%>%left_join(clustering.results%>%select(RegionName,cluster),by=c('RegionName'='RegionName'))
clustering.results.interpret=clustering.results.interpret%>%select(RegionName,most_variance_category,cluster)%>%pivot_longer(cols = most_variance_category)%>%group_by(cluster,name)%>%
  summarise(meanv=mean(value),maxv=max(value),minv=min(value))%>%ungroup()%>%pivot_longer(cols = meanv:minv,names_to='Measure')
p.intepreting=ggplot(clustering.results.interpret%>%filter(Measure=='meanv'),aes(x=0,y=value,fill=as.factor(cluster)))+
  geom_bar(stat='identity',position='dodge')+facet_wrap(~name)+theme_classic()
p.intepreting

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