# Help Desk Dataset.

library(dplyr)
library(funModeling)
library(stringr)
library(lubridate)

setwd("C:/Users/Tric/Downloads") #Working Directory
df <- read.csv("Help Desk.csv")
head(df)

# capture the basic structure, the field types, and other basic information.
str(df)


# identify fields with grouping data
#Issue Category
unique(df$issue_category)

#Owner Group
unique(df$owner_group)

# table the issue_category field for the issues shared in help desk.
table(df$issue_category)

# Average Issue day open days for the various issues categories
df %>%                    # with the sample
  group_by(issue_category) %>%   # group by issue_category
  summarise(Average_Issue_Open_day  = mean(days_open))  # summarise the groups by the day

# Relationship between Owner group  and the Issues categories fields
table(df$owner_group, df$issue_category)



# Relationship between ticket severity and the days it remains open
table(df$severity, df$days_open)

# convert the created date to date from chr to Date

df$created_date <- as.Date(df$created_date, "%m/%d/%Y")

# Relationship between ticket type and the month of ticket creation
table(df$ticket_type, month(df$created_date))

df %>%                                 # with the sample
  group_by(month(created_date)) %>%   # group by the month received
  summarise(Tickets_per_Month = n()) %>%       # summarise by counting what rows fall into each month
  arrange(desc(Tickets_per_Month))             # arrange into descending order


