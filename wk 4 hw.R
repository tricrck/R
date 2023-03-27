library(readxl)
library(DMwR)
library(dplyr)
library(openxlsx)

setwd(choose.dir())
df <- read_excel("Week_04_HW4_Titanic_Data(1).xls")

# Basic Information
str(df)

# Missing Values 
colSums(is.na(df))

# Remove missing values
df <- df %>% filter(!is.na(`Port of Embarkation`))
df <- df %>% filter(!is.na(`Passenger Fare`))

# Dropping Variables with more than 60% of the ,missing observations:
df <- df %>% select(-Cabin,-`Life Boat`)

# Impute missing values with the mean
mean_imputation <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

# Impute missing Age Values by Mean

df$Age <- mean_imputation(df$Age)
colSums(is.na(df))
str(df)

# Remove the duplicates based on the Ticket Number
dim(df)
df <- df[!duplicated(df[, c("Ticket Number")]), ]
dim(df)

# Remove spaces in column names
colnames(df) <- gsub(" ", "_", colnames(df))
colnames(df)

# Write the data to an excel file
write.xlsx(df, file = "titanic_cleaned.xlsx")
