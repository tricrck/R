library(tidyverse)
library('dplyr')

setwd(choose.dir())
cyber <- read.csv('salaries_cyber.csv')

# remove na if it exists
cyber <- cyber %>% filter(!is.na(salary_in_usd))

#Get Top thirty well paying Jobs
top_30 <- cyber %>% arrange(desc(salary_in_usd)) %>% head(30)

# Format date to get for the specific years
top_30$work_year <- paste(top_30$work_year, sep = "")

ggplot(data = top_30, aes(x = work_year, y = salary_in_usd, color = job_title)) +
  geom_point(aes(size = salary/1000)) +
  scale_size("Salary in Thousands", breaks = c(400, 500, 600, 700, 800, 900))+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(x = "Year",
       y = "Salary  Amount",
       title = "Cyber Security Salaries Time series",
       subtitle = "Cyber Security Salaries 2020 - 2022")
