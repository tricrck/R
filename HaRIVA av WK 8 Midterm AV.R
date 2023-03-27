require(ggplot2)
library(forcats)
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

#Plot pie chart for Average Home Prices
ggplot(avgHomeRates, aes(x = "", y = AveragePrice, fill = Location)) +
  geom_col(width = 1, position = "fill") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(100 * AveragePrice / sum(AveragePrice), 1), "%")),
            position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Proportion of Average Home Rates by Location",
       fill = "Location") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

#Get the average Crime rates in NY and FL.
avgCrimeRates <-
  df %>% group_by(Location) %>% summarize (AverageCrimeRate = mean(Crime.Rating, na.rm=TRUE))

#Print the average Crime rates
print(avgCrimeRates)

#Plot bar Graph for average Crime rates
ggplot(avgCrimeRates, aes(x = "", y = AverageCrimeRate, fill = Location)) +
  geom_col(width = 1, position = "fill") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(100 * AverageCrimeRate / sum(AverageCrimeRate), 1), "%")),
            position = position_fill(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Proportion of Average Crime Rates by Location",
       fill = "Location") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")



#Sort the data by Price
sortedByPrice <- df %>% arrange (-df$Price)

#Plot points is Graph for Crime Rates Vs Price
ggplot(data = sortedByPrice, aes(x = Price, y = Crime.Rating)) + 
  geom_point(aes(color= Location, size = Crime.Rating), alpha = 0.7) + 
  scale_x_continuous(labels=scales::dollar_format()) + 
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Crime Rates Vs Price by Location", 
       x = "Price", y = "Crime Rating", 
       size = "Crime Rating", color = "Location") +
  theme_bw()

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

ggplot(HousePayBal, aes(x = fct_reorder(Location, -HousePayBal), y = HousePayBal, fill = Location)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = sprintf("$%1.0fk", HousePayBal/1000)), 
            vjust = -0.5, size = 4, color = "white") +
  theme(legend.position = "none") + 
  labs(title = "House Payment Balances with 100% of Salary on House Payments",
       x = "Location",
       y = "House Payment Balance") +
  coord_flip()


