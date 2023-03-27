library(tidyverse)
library('dplyr')

setwd("C:/Users/Tric/Downloads")
cars <- read.csv('Car details v3.csv')

# Add 15 more years to the cars dataset used for analysis and viSualization.
cars1 <- cars %>% 
  filter(year >= "2000")
cars1$year <- as.character(cars1$year)



#select the used honda cars from the dataset

honda <- cars1[grep("^Honda", cars1$name), ]

ggplot(data = honda, aes(x = year, y = selling_price)) +
  geom_bar(aes(fill = fuel),stat = "identity",
           position = position_dodge(0.8),
           width = 0.7) +
  scale_y_continuous(labels=scales::dollar_format())+
  labs(x = "Year",
       y = "Selling Price",
       title = "Used Honda Cars selling Price with their Fuel Types",
       subtitle = "Honda Selling Prices between 2015 - 2020")

honda1 <- honda %>% 
  filter(selling_price < 400000)
honda1$name <- sub("Honda ", "", honda1$name)

ggplot(data = honda1, aes(x = name, y = selling_price)) +
  geom_bar(aes(fill = fuel),stat = "identity",
           position = position_dodge(0.6),
           width = 0.7) +
  scale_y_continuous(labels=scales::dollar_format())+
  labs(x = "Honda Brand",
       y = "Selling Price",
       title = "Used Honda Cars selling Price less than 400000 with their Fuel Types",
       subtitle = "Honda Selling Prices between 2000 - 2020")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



