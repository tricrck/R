library(ggplot2)
library(scales)
library(quantmod)
library(ggalt)
library(tidyr)
library(dplyr)
library(CGPfunctions)

setwd(choose.dir())

# read data
df <- read.csv("Telecom Churn Dataset Arun.csv", sep = ",")

## Quiz6

# Using the Tenure as the time  in the dataset
ggplot(df, aes(x = Tenure, y = TotalCharges)) +
  geom_line() +
  labs(title = "Total Charges Rate",
       x = "Tenure",
       y = "Total Charges Rate")

ggplot(df, aes(x = Tenure, y = TotalCharges)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  labs(title = "Total Charges Rate",
       subtitle = "Tenure term",
       x = "",
       y = "Total Charges Rate") +
  theme_minimal()




# convert data to wide format
plotdata_wide <- spread(df, InternetService, Churn)
names(plotdata_wide) <- c("InternetService", "Yes", "No")

# create dumbbell plot
ggplot(plotdata_wide, aes(y = InternetService,
                          x = Yes,
                          xend = No)) +  
  geom_dumbbell()

# create dumbbell plot
ggplot(plotdata_wide, 
       aes(y = reorder(InternetService, Yes),
           x = Yes,
           xend = No)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "blue", 
                colour_xend = "red") +
  theme_minimal() + 
  labs(title = "Change in InternetService",
       subtitle = "Telecom Dataset Churn Prediction",
       x = "",
       y = "")


# create slope graph
newggslopegraph(df, Married, SeniorCitizen, Churn) +
  labs(title="Senior Citizen by Churn", 
       subtitle="Telecom Churn Dataset", 
       caption="source: Telecom Churn Dataset")

# area chart
ggplot(df, aes(x = Tenure, y = TotalCharges)) +
  geom_area(fill="lightblue", color="black") +
  labs(title = "Personal Savings Rate",
       x = "Tenure",
       y = "Personal Savings Rate")
