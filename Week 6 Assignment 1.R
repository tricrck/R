# Call Center Dataset.

library(dplyr)
library(ggplot2)
setwd("C:/Users/Tric/Downloads")
df <- read.csv("Call Center.csv", header=TRUE, stringsAsFactors=FALSE)
head(df)

# Call time stamp reassign datatype as Date 
df$call_timestamp <- as.Date(df$call_timestamp, "%m/%d/%Y")

# Box Plot of Sentiment against the csat_score

ggplot(df, aes(x=sentiment, y=csat_score, colour=channel)) +
  geom_boxplot()+ # Show dots
  theme(axis.text.x = element_text(angle=40, vjust=0.6)) +
  geom_text(data=df, aes( x="Very Positive", y=10, label="best outcome"),                 , 
            color="orange", 
            size=7)+
  labs(title="Box Plot of Sentiment aginst the csat_score", 
       subtitle = "Sentiment aginst the csat_score", 
       y="csat_score", 
       caption="Source: Tableau Data Sets")

# Box Plot of Call duration in Minutes against Resason

ggplot(df, aes(x=reason, y=call.duration.in.minutes, colour=response_time)) +
  geom_boxplot()+ # Show dots
  theme(axis.text.x = element_text(angle=40, vjust=0.6)) + 
  labs(title="Box Plot of Call duration in Minutes against Resason", 
       subtitle = "Call duration in Minutes against Resason", 
       y="Call duration in Minutes", 
       caption="Source: Tableau Data Sets")+
  annotate("text", x = "Payments", y = 40, label = "Longest Payment calls", colour="green", size=3)

# Bar Plot of Overall Sentiment to their reasons for call

ggplot(df) +
  geom_bar(aes(sentiment, fill=reason))+
  theme(axis.text.x = element_text(angle=40, vjust=0.6)) + 
  labs(title="Bar Plot of Overall Sentiment to their reasons for call", 
       subtitle = "Sentiment ration in response to reason", 
       y="Total Calls per sentiment", 
       caption="Source: Tableau Data Sets")+
  annotate("text", x = "Negative", y = 11000, label = "Billing questions has highest negative sentiment", colour="black", size=3)
