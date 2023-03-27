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
ggplot(avgHomeRates, aes(x = "", y = AveragePrice, fill = Location)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(round(100 * AveragePrice / sum(AveragePrice), 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Average Home Rates") +
  theme(plot.title = element_text(hjust = 0.5))
#Get the average Crime rates in NY and FL.
avgCrimeRates <-
  df %>% group_by(Location) %>% summarize (AverageCrimeRate = mean(Crime.Rating, na.rm=TRUE))

#Print the average Crime rates
print(avgCrimeRates)

#Plot bar Graph for average Crime rates

ggplot(avgCrimeRates, aes(x = "", y = AverageCrimeRate, fill = Location)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(round(100 * AverageCrimeRate / sum(AverageCrimeRate), 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Average Crime Rates") +
  theme(plot.title = element_text(hjust = 0.5))

#Sort the data by Price
sortedByPrice <- df %>% arrange (-df$Price)

#Plot points is Graph for Crime Rates Vs Price
ggplot(data = sortedByPrice, aes(x = Price, y = Crime.Rating)) + 
  geom_point(aes(color= Location, size = Crime.Rating),stat = "identity") + 
  scale_x_continuous(labels=scales::dollar_format()) 
  ggtitle("Crime Rates Vs Price")

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

