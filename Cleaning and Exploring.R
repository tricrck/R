library(dplyr)
library(funModeling)
library(stringr)
library(lubridate)

# Accessing the datasetto R dataframe df
df<- read.csv("Human Resources.csv", header=TRUE, stringsAsFactors=FALSE)


head(df)


# Data frame Structure
str(df)

# Data cleaning Date fields assigned as character 
# birth date datatype as Date
df$birthdate <- as.Date(df$birthdate, "%m/%d/%Y")
# hire_date datatype as Date
df$hire_date <- as.Date(df$hire_date, "%m/%d/%Y")
# Factorize race field 
df$race <- as.factor(df$race)
# Factorize race field 
df$location <- as.factor(df$location)
str(df)

df_status(df)

# Relationship between hire date and gender
table(year(df$hire_date), df$gender)

# Relationship between hire date and race
table(year(df$hire_date), df$race)

# Relationship between hire department and location
table(df$department, df$location)

# Relationship between hire_date and location
table(months(df$hire_date), df$location)

