# Week Four Midterm
##
require(ggplot2)
require(dplyr)
require(gridExtra)
library(xlsx)
options (scipen = 10)

setwd(choose.dir())
df <- read.xlsx("File for Midterm (2).xlsx", sheetName="Sheet1")



#1. Get the average home Prices in NY and FL.
home_rates <-
  df %>% group_by(Location) %>% summarize(AveragePrice = mean(Price, na.rm= TRUE))

print(home_rates)


#Plot bar Graph for Average Home Prices
ggplot(data = home_rates, aes(x = Location, y = AveragePrice, fill=Location)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels=scales::dollar_format())+
  labs(x = "Location",
       y = "Average House Price") +
  scale_fill_manual(name = "Location", values = c("cyan", "gray")) +
  ggtitle("Average Home Prices") 

# 2. Get the average Crime rates in NY and FL.
crime_rates <-
  df %>% group_by(Location) %>% summarize (AverageCrimeRate = mean(Crime.Rating, na.rm=TRUE))

#Print the average Crime rates
print(crime_rates)

#Plot bar Graph for average Crime rates
ggplot(data = crime_rates, aes(x = Location, y = AverageCrimeRate, fill=Location)) + 
  geom_bar(stat = "identity") +
  labs(x = "Location",
       y = "Average Crime Rate") + theme (legend.position = "none") + 
  ggtitle("Average Crime Rates") + scale_fill_manual(name = "Location", values = c("cyan", "gray"))


#Sort the data by Price
sortedByPrice <- df %>% arrange (-df$Price)

# 3. Plot points is Graph for Crime Rates Vs Price
ggplot(data = sortedByPrice, aes(x = Price, y = Crime.Rating)) + 
  geom_point(aes(color= Location), stat = "identity", size = 3, shape = 17) + 
  scale_x_continuous(labels=scales::dollar_format())+
  ggtitle("Crime Rates Vs Price")

# Question 5 
#She has $100,000 to put down for the house.
#If she moves to NY she will have a job earning $120,000 per year.
#If she moves to FL she will have a job earning $75,000 per year
#Invest Amount
invest_amount = 100000

#Get the avarege home rates for Florida

Avg_Home_Rates_Florida = as.numeric(home_rates[1, 2])

#Get the avarege home rates for NewYork

Avg_Home_Rates_NewYork = as.numeric(home_rates[2, 2])

print(paste ("Average Home Price in Florida is: ", format(
  round (Avg_Home_Rates_Florida, 2), nsmall = 2)))

print(paste ("Average Home Price in NewYork is: ", format(
  round (Avg_Home_Rates_NewYork, 2), nsmall = 2)))

#Assuming 40% of salary for home payment
salary_florida = 75000 * 1.0

salary_new_york = 120000 * 1.0

#House  repayment years in Florida
Repayment_Years_Florida = (invest_amount + salary_florida)

#House repayment years in NewYork
Repayment_Years_NewYork = (invest_amount + salary_new_york)

x <- c("NY Total", "NY Average", "FL Total", "FL Average")

HousePayBal<- data.frame(Location = x, HousePrice = c(Repayment_Years_NewYork, round(Avg_Home_Rates_NewYork, 2), Repayment_Years_Florida, round(Avg_Home_Rates_Florida, 2)))

ggplot(HousePayBal, aes(x = Location, y = HousePrice)) + 
  geom_bar(aes(fill = Location), stat = "identity")+
  stat_summary(fun=mean, geom="text", vjust=-0.5, aes(label=HousePrice), show.legend = FALSE)+
  ggtitle("Investment plus Salary Totals Thousands compared to averages") + scale_fill_manual(name = "Location", values = c("red", "blue", "green", "yellow"))

ggplot(df, aes(x = Location, y = Price)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  scale_fill_hue (c = 50) + theme (legend.position = "none") + 
  ggtitle("House Price Boxplots")

