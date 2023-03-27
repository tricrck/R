# Call Center Dataset.
#install.packages('funModeling')

library(dplyr)
library(funModeling)
library(stringr)
library(lubridate)

df <- read.csv("Call Center.csv", header=TRUE, stringsAsFactors=FALSE)
head(df)

# capture the basic structure, the field types, and other basic information.
str(df)

table(str(df))

# Call time stamp reassign datatype as Date 
df$call_timestamp <- as.Date(df$call_timestamp, "%m/%d/%Y")

# identify fields with grouping data
df_status(df)

# identify fields with grouping data
unique(df$csat_score)

# table the reason field for the callers to the call center.
table(df$call.duration.in.minutes)

# Average Call duration for the various caller reasons.
df %>%                    # with the sample
  group_by(reason) %>%   # group by reason
  summarise(Average_Call_Duration  = mean(call.duration.in.minutes))  # summarise the groups by the caller sentiment

# Relationship between caller reason and the sentiment
table(df$reason, df$sentiment)



# Relationship between channel and call center
table(df$channel, df$call_center)



# Relationship between caller reason and the day of call
table(df$reason, day(df$call_timestamp))

df %>%                               
  group_by(day(call_timestamp)) %>% 
  summarise(Calls_per_day = n()) %>%  
  arrange(desc(Calls_per_day))             # arrange into descending order


df %>% 
  filter(str_detect(string = channel, 
                    pattern = "Email")) %>% 
  {unique(.$channel)}

df2 <- df %>% filter(csat_score >= 10)
table(df2$state, df2$csat_score)
