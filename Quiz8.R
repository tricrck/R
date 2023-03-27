#Loading Packages
library(tidyverse)
library(scales)
library(ggridges)
library(ggcorrplot)
library(mosaicData)
library(scatterplot3d)

setwd(choose.dir())

# read data
df <- read.csv("Telecom Churn Dataset Arun.csv", sep = ",")

## Quiz8

# basic 3-D scatterplot
with(df, {
  scatterplot3d(x = MonthlyCharges,
                y = TotalCharges, 
                z = Tenure,
                main="3-D Scatterplot Example 1")
  })

  
  # create a biplot
  # load data
  
  # fit a principal components model
df1 <- dplyr::select_if(df, is.numeric)
  fit <- prcomp(x = df1, 
                center = TRUE, 
                scale = TRUE)
  
  # plot the results
  library(factoextra)
  fviz_pca(fit, 
           repel = TRUE, 
           labelsize = 3) + 
    theme_bw() +
    labs(title = "Biplot of Telecom Dataset Churn Prediction numeric data")
  
  # create a bubble plot
  ggplot(df, 
         aes(x = Tenure, y = TotalCharges, size = SeniorCitizen)) +
    geom_point()
  
  # create a bubble plot with modifications
  ggplot(df, 
         aes(x = Tenure, y = TotalCharges, size = SeniorCitizen)) +
    geom_point(alpha = .5, 
               fill="cornflowerblue", 
               color="black", 
               shape=21) +
    scale_size_continuous(range = c(1, 14)) +
    labs(title = "bubble plot with modifications",
         subtitle = "Total Charges by Tenure",
         x = "Tenure",
         y = "TotalCharges",
         size = "SeniorCitizen") 
  
  # create Sankey diagram
  library(networkD3)
  sankeyNetwork(Links = links, 
                Nodes = nodes, 
                Source = "source",
                Target = "target", 
                Value = "value", 
                NodeID = "name",
                units = "TWh", # optional units name for popups
                fontSize = 12, 
                nodeWidth = 30)
  
 

  ggplot(df,
         aes(axis1 = Married,
             axis2 = SeniorCitizen,
             y = n)) +
    geom_alluvium(aes(fill = Gender)) +
    geom_stratum() +
    geom_text(stat = "stratum", 
              label.strata = TRUE) +
    scale_x_discrete(limits = c("Married", "SeniorCitizen"),
                     expand = c(.1, .1)) +
    labs(title = "Titanic data",
         subtitle = "stratified by class, sex, and survival",
         y = "Frequency") +
    theme_minimal()
  
  # create alternative alluvial diagram
  library(ggplot2)
  library(ggalluvial)
  ggplot(titanic_table,
         aes(axis1 = Class,
             axis2 = Sex,
             axis3 = Survived,
             y = n)) +
    geom_alluvium(aes(fill = Class)) +
    geom_stratum() +
    geom_text(stat = "stratum", 
              label.strata = TRUE) +
    scale_x_discrete(limits = c("Class", "Sex", "Survived"),
                     expand = c(.1, .1)) +
    scale_fill_viridis_d() +
    labs(title = "Titanic data",
         subtitle = "stratified by class, sex, and survival",
         y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none") 
  
  # create a heatmap
  library(superheat)
  superheat(df1, scale = TRUE)
  
  # sorted heat map
  superheat(df1,
            scale = TRUE,
            left.label.text.size=3,
            bottom.label.text.size=3,
            bottom.label.size = .05,
            row.dendrogram = TRUE )
  
######
        
  
  install.packages("devtools")
  devtools::install_github("ricardo-bion/ggradar")
  
  # create a radar chart
  
  # prepare data
  data(msleep, package = "ggplot2")
  library(ggradar)
  library(scales)
  library(dplyr)
  
  plotdata <- df %>%
    filter(InternetService %in% c("DSL", "Fiber Optics", "NO")) %>%
    select(InternetService, MonthlyCharges, 
           Tenure, MonthlyCharges) %>%
    rename(group = InternetService) %>%
    mutate_at(vars(-group),
              funs(rescale))
  plotdata
  
  # generate radar chart
  ggradar(plotdata, 
          grid.label.size = 4,
          axis.label.size = 4, 
          group.point.size = 5,
          group.line.width = 1.5,
          legend.text.size= 10)
  
  library(GGally)
  
  # prepare data
  data(df, package="ggplot2")
  library(dplyr)
  dfa <- df %>% 
    mutate(log_MonthlyCharges = log(MonthlyCharges),
           log_TotalCharges = log(TotalCharges)) %>%
    select(log_MonthlyCharges, log_TotalCharges, Tenure, SeniorCitizen   )
  
  
  # create a scatterplot matrix
  ggpairs(dfa)
  
  # custom function for density plot
  my_density <- function(data, mapping, ...){
    ggplot(data = data, mapping = mapping) + 
      geom_density(alpha = 0.5,
                   fill = "cornflowerblue", ...)
  }
  
  # custom function for scatterplot
  my_scatter <- function(data, mapping, ...){
    ggplot(data = data, mapping = mapping) + 
      geom_point(alpha = 0.5,
                 color = "cornflowerblue") + 
      geom_smooth(method=lm, 
                  se=FALSE, ...)
  }
  
  
  # create scatterplot matrix
  ggpairs(df, 
          lower=list(continuous = my_scatter), 
          diag = list(continuous = my_density)) +
    labs(title = "Mammal size and sleep characteristics") +
    theme_bw()
  
  # create \
  category <- df$InternetService
  amount <- df$TotalCharges
  net <- data.frame(category, amount) 
  
  # create waterfall chart
  library(ggplot2)
  library(waterfalls)
  waterfall(net) 
  
  # install packages for text mining
  install.packages(c("tm", "SnowballC", 
                     "wordcloud", "RColorBrewer", 
                     "RCurl", "XML"))
  # create a word cloud
  script <- "http://www.sthda.com/upload/rquery_wordcloud.r"
  source(script)
  res<-rquery.wordcloud("JFKspeech.txt", 
                        type ="file", 
                        lang = "english")
  