library(shiny)
library(tidyverse)
library('dplyr')

setwd("C:/Users/Tric/Downloads")
cyber <- read.csv('salaries_cyber.csv')

## Adding data to cyber Salaries for 2017, 2018, 2019
list_size = length(cyber[1][[1]])
work_year = list(cyber$work_year)
experience_level = list(cyber$experience_level)
employment_type = list(cyber$employment_type)
job_title = list(cyber$job_title)
salary = list(cyber$salary)
salary_currency = list(cyber$salary_currency)
salary_in_usd = list(cyber$salary_in_usd)
employee_residence = list(cyber$employee_residence)
remote_ratio = list(cyber$remote_ratio)
company_location = list(cyber$company_location)
company_size = list(cyber$company_size)

salary_to_usd <- function(salary, salary_currency, index){
  currency <- salary_currency[[1]][index]
  sal = salary[[1]][index]
  tmp = cyber%>%select(salary, salary_currency, salary_in_usd)%>%filter(salary_currency == currency)
  # turnoff scientific notation
  options(scipen = 999)
  if (currency == "USD"){
    return(sal)
  }
  else {
    sal_in_usd = sal/(tmp[1,1]/tmp[1,3])
    return(round(sal_in_usd,0))
  }
}

for (i in 1:5000) {
  experience_level[[1]][i+list_size] = experience_level[[1]][sample(1:list_size, 1)]
  employment_type[[1]][i+list_size] = employment_type[[1]][sample(1:list_size, 1)]
  job_title[[1]][i+list_size] = job_title[[1]][sample(1:list_size, 1)]
  employee_residence[[1]][i+list_size] = employee_residence[[1]][sample(1:list_size, 1)]
  company_location[[1]][i+list_size] = company_location[[1]][sample(1:list_size, 1)]
  company_size[[1]][i+list_size] = sample(c("S", "M", "L"), 1)
  salary[[1]][i+list_size] = sample(2000:910991, 1)
  salary_currency[[1]][i+list_size] = sample(
    c(
      "USD",
      "BRL",
      "GBP",
      "EUR",
      "INR",
      "CAD",
      "CHF",
      "DKK",
      "SGD",
      "AUD",
      "SEK",
      "MXN",
      "ILS",
      "PLN",
      "NOK",
      "IDR",
      "NZD",
      "HUF",
      "ZAR",
      "TWD",
      "RUB"),1)
  remote_ratio[[1]][i+list_size] = sample(c(0,50,100), 1)
  work_year[[1]][i+list_size] = sample(c(2017,2018,2019),1)
  salary_in_usd[[1]][i+list_size] = salary_to_usd(salary, salary_currency, i+list_size)
}

df <- data.frame(work_year, experience_level, employment_type, job_title, salary, salary_currency, salary_in_usd, employee_residence,
                remote_ratio, company_location, company_size)
colnames(df) <- c("work_year", "experience_level", "employment_type", "job_title", "salary", "salary_currency", "salary_in_usd", "employee_residence","remote_ratio", "company_location", "company_size")

cyber <- df%>%filter(salary_in_usd>1000)


# User Interface
ui <- basicPage(
  
  sliderInput("work_year", "Select year:", 
              animate = TRUE,
              min = 2017, max = 2022, value = 2022,
              step = 1,
              # so thousands are not separated with a comma
              # (without sep = "" the scale defaults to 1,952 - 2,007)
              sep = ""   
  ),# note this comma here - different to our usual R code
  plotOutput(outputId = "myplot")
)
# Server
server <- function(input, output) {
  output$myplot <- renderPlot({
    #your plot code here:
    cyber %>%
      filter(work_year == input$work_year) %>%
      filter(experience_level == "EN") %>%
      ggplot(aes(y = salary, x = company_location, colour = employment_type)) + 
      geom_point(aes(size = salary/1000))+
      scale_size("Salary in Thousands", breaks = c(100, 200, 300))+
      scale_fill_distiller(palette = "Paired") +
      theme_bw() +
      labs(main = "Relationship of Salaries to various Locations in relation to Work Type", caption = "Each bubble is a Salary.", ylab = "Salary")  +
      theme(aspect.ratio=0.6)
  })  
}
shinyApp(ui, server)