# Dimension reduction for viz (40800)
# Acknoldegments:  # Philip Waggoner, pdwaggoner@uchicago.edu

# Techniques: t-SNE, UMAP

#
# t-SNE first

# libraries needed for this section
library(tidyverse)
library(here)
library(amerika)
library(tictoc)
library(patchwork)
library(Rtsne)
library(umap)
containment_measures=c('C1_School closing','C2_Workplace closing','C3_Cancel public events','C4_Restrictions on gatherings',
                       'C5_Close public transport','C6_Stay at home requirements','C7_Restrictions on internal movement',
                       'C8_International travel controls')
economic_measures=c('E1_Income support','E2_Debt/contract relief','E3_Fiscal measures')

health_measures=c('H1_Public information campaigns','H2_Testing policy','H3_Contact tracing','H4_Emergency investment in healthcare',
                  'H5_Investment in vaccines','H6_Facial Coverings',
                  'H8_Protection of elderly people')
country_variables=c('CountryName','CountryCode','RegionName','Date')
outcome_variables=c('ConfirmedDeaths','ConfirmedCases')

remove=c('E3_Fiscal measures','H4_Emergency investment in healthcare','H5_Investment in vaccines')
classification=c(containment_measures,economic_measures,health_measures)
classification=classification[!classification%in%remove]

setwd('C:\\repos\\covid-measures-uml\\')
load('C:\\repos\\covid-measures-uml\\data\\MeasuresAroundPeakDf.Rdata')
df.aroundpeak.meanmeasures.states=df.aroundpeak.meanmeasures%>%rename('maxdeaths'='newdeaths')%>%left_join(state_variables)%>%mutate(deaths_1000=(maxdeaths*1000)/(population))

#Fit algorithms
set.seed(1234)
dimension_variables=
tsne_2 <- Rtsne(as.matrix(df.aroundpeak.meanmeasures.states[ ,classification]), perplexity = 2)
  # perplexity = 2
  tsne_2 <- Rtsne(as.matrix(anes[ ,1:35]), 
                  perplexity = 2)

  perp_1 <- ggplot(data=df.aroundpeak.meanmeasures.states,aes(x=tsne_2$Y[,1],y= tsne_2$Y[,2], 
               col = factor(party))) +
    geom_point(size=2.5) +
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Party",
                       breaks=c("republican", "democrat"),
                       labels=c("republican", 
                                "democrat")) +
    labs(x = "First dimension",
         y = "Second dimension",
         subtitle = "Perplexity = 2") +
    theme_minimal()
  perp_1
  # perplexity = 25
    perp_2=10
  tsne_25 <- Rtsne(as.matrix(df.aroundpeak.meanmeasures.states[ ,classification]), 
                   perplexity = perp_2) 
  
  perp_2 <- ggplot(data=df.aroundpeak.meanmeasures.states,aes(x=tsne_25$Y[,1],y= tsne_2$Y[,2], 
                                                              col = factor(party))) +
    geom_point(size=2.5) +
    stat_ellipse() +
    scale_color_manual(values=c(amerika_palettes$Republican[1], 
                                amerika_palettes$Democrat[1]),
                       name="Party",
                       breaks=c("republican", "democrat"),
                       labels=c("republican", 
                                "democrat")) +
    labs(x = "First dimension",
         y = "Second dimension",
         subtitle = paste0("Perplexity = ",perp_2)) +
    theme_minimal()
  perp_2
  
 
# Visualize
tsne_plots <- (perp_1 + perp_2) 
tsne_plots

# UMAP next: in general not the best results
# epochs = 500
umap_fit_5 <- df.aroundpeak.meanmeasures.states[ ,classification] %>% 
  umap(n_neighbors = 6,
       metric = "euclidean",
       n_epochs = 100)

umap_fit <- df.aroundpeak.meanmeasures.states %>% 
  mutate(First_Dimension = umap_fit_5$layout[,1],
         Second_Dimension = umap_fit_5$layout[,2]) %>% gather(key = "Variable",
         value = "Value",
         c(-First_Dimension, -Second_Dimension, -party))

k_5 <- ggplot(umap_fit, aes(First_Dimension, Second_Dimension, 
                              col = factor(party))) + 
  geom_point(alpha = 0.6,size=3) +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Party",
                     breaks=c("republican", 
                              "democrat"),
                     labels=c("replublican", 
                              "democrat")) +
  labs(title = " ",
       subtitle = "Neighborhood size: 5; Epochs = 500",
       x = "First Dimension",
       y = "Second Dimension") +
  theme_minimal()
k_5



#OLD
{
# epochs = 20 
umap_fit_e_20 <- anes[,1:35] %>% 
  umap(n_neighbors = 5,
       metric = "euclidean",
       n_epochs = 20)

umap_fit_e_20 <- anes %>% 
  mutate(First_Dimension = umap_fit_e_20$layout[,1],
         Second_Dimension = umap_fit_e_20$layout[,2]) %>% 
  gather(key = "Variable",
         value = "Value",
         c(-First_Dimension, -Second_Dimension, -democrat))

e_20 <- ggplot(umap_fit_e_20, aes(First_Dimension, Second_Dimension, 
                                  col = factor(democrat))) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(values=c(amerika_palettes$Republican[1], 
                              amerika_palettes$Democrat[1]),
                     name="Democrat",
                     breaks=c("0", 
                              "1"),
                     labels=c("No", 
                              "Yes")) +
  labs(title = " ",
       subtitle = "Neighborhood size: 5; Epochs = 20",
       x = "First Dimension",
       y = "Second Dimension") +
  theme_minimal()

# side by side
k_5 + e_20
}
