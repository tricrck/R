require(ggplot2)
require(dplyr)
require(gridExtra)
library(xlsx)

setwd(choose.dir())
df <- read.xlsx("File for Midterm (1)(1) (1).xlsx", sheetName="Sheet1")



#Get the average home Prices in NY and FL.
avgHomeRates <-
  df %>% group_by(Location) %>% summarize(AveragePrice = mean(Price, na.rm= TRUE))

print(avgHomeRates)

#prevents scales in scientific notation
options (scipen = 10)

#Plot bar Graph for Average Home Prices
plot1 <-
  ggplot(data = avgHomeRates, aes(x = Location, y = AveragePrice, fill=Location)) + 
  geom_bar(stat = "identity") + 
  scale_fill_hue (c = 50) + theme (legend.position = "none") +
  ggtitle("Average Home Prices")

#Get the average Crime rates in NY and FL.
avgCrimeRates <-
  df %>% group_by(Location) %>% summarize (AverageCrimeRate = mean(Crime.Rating, na.rm=TRUE))

#Print the average Crime rates
print(avgCrimeRates)

#Plot bar Graph for average Crime rates
plot <-
  ggplot(data = avgCrimeRates, aes(x = Location, y = AverageCrimeRate, fill=Location)) + 
  geom_bar(stat = "identity") + 
  scale_fill_hue (c = 50) + theme (legend.position = "none") + 
  ggtitle("Average Crime Rates")


#Sort the data by Price
sortedByPrice <- df %>% arrange (-df$Price)

#Plot points is Graph for Crime Rates Vs Price
plot0 <-
  ggplot(data = sortedByPrice, aes(x = Price, y = Crime.Rating, fill= Location)) + 
  geom_point(stat = "identity") + 
  scale_fill_hue (c = 50) + theme (legend.position = "none") + 
  ggtitle("Crime Rates Vs Price")

#Use grid to arrange the plots
grid.arrange (plot1, plot, plot0)

#Invest Amount
investAmount = 100000

#Get the avarege home rates for Florida

AverageFloridaHousePrice = as.numeric(avgHomeRates[1, 2])

#Get the avarege home rates for NewYork

AverageNewYorkHousePrice = as.numeric(avgHomeRates[2, 2])

print(paste ("Average Home Price in Florida is: ", format(
  round (AverageFloridaHousePrice, 2), nsmall = 2)))

print(paste ("Average Home Price in NewYork is: ", format(
  round (AverageNewYorkHousePrice, 2), nsmall = 2)))

#Assuming 40% of salary for home payment
salaryFlorida = 75000 * 1.0

salaryNewYork = 120000 * 1.0

#House  repayment years in Florida
HousePayFloridaBal = (AverageFloridaHousePrice - investAmount - salaryFlorida)

#House repayment years in NewYork
HousePayNewYorkBal = (AverageNewYorkHousePrice - investAmount - salaryNewYork)

x <- c("NY", "FL")

HousePayBal<- data.frame(Location = x, HousePayBal = c(HousePayNewYorkBal, HousePayFloridaBal))

plotavg <-
  ggplot(HousePayBal, aes(x = Location, y = HousePayBal, fill = Location)) + 
  geom_bar(stat = "identity") + 
  scale_fill_hue (c = 50) + theme (legend.position = "none") + 
  ggtitle("House Payment Balances with 100% of Salary on House payments")

