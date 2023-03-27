# 1. The student directory for a large university has 400 pages with 130 names per page, 
    #a total of 52,000 names. Using software, show how to select a simple random sample of 10 names.
 # Randomly generate the student names dataframe
first <- c("Fear", "Frontier", "Nanny", "Job", "Yard", "Airport", "Half Pint", "Commando", "Fast Food", "Basketball", "Bachelorette", "Diva", "Baggage", "College", "Octane", "Clean", "Sister", "Army", "Drama", "Backyard", "Pirate", "Shark", "Project", "Model", "Survival", "Justice", "Mom", "New York", "Jersey", "Ax", "Warrior", "Ancient", "Pawn", "Throttle", "The Great American", "Knight", "American", "Outback", "Celebrity", "Air", "Restaurant", "Bachelor", "Family", "Royal", "Surf", "Ulitmate", "Date", "Operation", "Fish Tank", "Logging", "Hollywood", "Amateur", "Craft", "Mystery", "Intervention", "Dog", "Human", "Rock", "Ice Road", "Shipping", "Modern", "Crocodile", "Farm", "Amish", "Single", "Tool", "Boot Camp", "Pioneer", "Kid", "Action", "Bounty", "Paradise", "Mega", "Love", "Style", "Teen", "Pop", "Wedding", "An American", "Treasure", "Myth", "Empire", "Motorway", "Room", "Casino", "Comedy", "Undercover", "Millionaire", "Chopper", "Space", "Cajun", "Hot Rod", "The", "Colonial", "Dance", "Flying", "Sorority", "Mountain", "Auction", "Extreme", "Whale", "Storage", "Cake", "Turf", "UFO", "The Real", "Wild", "Duck", "Queer", "Voice", "Fame", "Music", "Rock Star", "BBQ", "Spouse", "Wife", "Road", "Star", "Renovation", "Comic", "Chef", "Band", "House", "Sweet")
second <- c("Hunters", "Hoarders", "Contest", "Party", "Stars", "Truckers", "Camp", "Dance Crew", "Casting Call", "Inventor", "Search", "Pitmasters", "Blitz", "Marvels", "Wedding", "Crew", "Men", "Project", "Intervention", "Celebrities", "Treasure", "Master", "Days", "Wishes", "Sweets", "Haul", "Hour", "Mania", "Warrior", "Wrangler", "Restoration", "Factor", "Hot Rod", "of Love", "Inventors", "Kitchen", "Casino", "Queens", "Academy", "Superhero", "Battles", "Behavior", "Rules", "Justice", "Date", "Discoveries", "Club", "Brother", "Showdown", "Disasters", "Attack", "Contender", "People", "Raiders", "Story", "Patrol", "House", "Gypsies", "Challenge", "School", "Aliens", "Towers", "Brawlers", "Garage", "Whisperer", "Supermodel", "Boss", "Secrets", "Apprentice", "Icon", "House Party", "Pickers", "Crashers", "Nation", "Files", "Office", "Wars", "Rescue", "VIP", "Fighter", "Job", "Experiment", "Girls", "Quest", "Eats", "Moms", "Idol", "Consignment", "Life", "Dynasty", "Diners", "Chef", "Makeover", "Ninja", "Show", "Ladies", "Dancing", "Greenlight", "Mates", "Wives", "Jail", "Model", "Ship", "Family", "Videos", "Repo", "Rivals", "Room", "Dad", "Star", "Exes", "Island", "Next Door", "Missions", "Kings", "Loser", "Shore", "Assistant", "Comedians", "Rooms", "Boys")

# for loop to generate 52000 random names based on the dictionary.
names = list()
for (x in 1:52000) {
  rand1 <- sample(1:length(first), 1)
  rand2 <- sample(1:length(second), 1)
  names[x] <- paste(first[rand1], second[rand2], sep = " ")
}
# name the generated student directory df.
df <- data.frame(matrix(unlist(names), nrow=length(names), byrow=TRUE))
colnames(df) <- c("StudentNames")
head(df, 10)


### Using software, show how to select a simple random sample of 10 names.

df[sample(nrow(df), 10), ]



# 2.From the Murder data file, use the variable murder, which is the murder rate (per 100,000 population) 
      # for each state in the U.S. in 2017 according to the FBI Uniform Crime Reports. 
      # At first, do not use the observation for D.C. (DC). Using software:
murder <- read.table("http://stat4ds.rwth-aachen.de/data/Murder.dat", 
                      header=TRUE)
head(murder, 10)

# removing DC from observation which is observation number 51
murder1 <- murder[-c(51), ]

# a. Find the mean and standard deviation and interpret their values.

mean(murder1$murder)

sd(murder1$murder)
#
#
## From the mean and standard deviation we observed that the mean murder is 4.874 in the U.S. 
#  in 2017 according to the FBI Uniform Crime Reports which represents the central tendancy of murder which low and,
#   a standard deviation of 2.586291, which is low and more data is clustered around the mean.
#
#

# b. Find the five-number summary, and construct the corresponding boxplot.
# Five summary with R's fivenum()
fivenum(murder1$murder)

# construct the corresponding boxplot

boxplot(murder1$murder,horizontal=TRUE)
text(x=fivenum(murder1$murder),labels=fivenum(murder1$murder),y=1.25)

## From the mean and standard deviation we observed that the mean murder is 4.874

# c. Now include the observation for D.C. What is affected more by this outlier: The mean or the median? 
# mean deviation
# use murder dataframe as it included D.C
mean(murder$murder) - mean(murder1$murder)
# median deviation
median(murder$murder) - median(murder1$murder)

#
#
## The mean is affected more by this outlier compared to the median
#
#

### 3.The Houses data file lists the selling price (thousands of dollars), size (square feet), tax bill (dollars)
    # , number of bathrooms, number of bedrooms, and whether the house is new (1 = yes,0 = no) 
    # for 100 home sales in Gainesville, Florida. Let’s analyze the selling prices.

houses <- read.table("http://stat4ds.rwth-aachen.de/data/Houses.dat", 
                     header=TRUE)
head(houses, 10)

#
# a. Construct a frequency distribution and a histogram.

# Frequency distribution of prices table
transform(table(houses$price))
# Histogram of the Selling Prices
bins <- seq(31.5, 131.5, by = 200)
hist(houses$price, breaks = bins, main = "Histogram of the Selling Prices", xlab = " Selling Prices ", col = "blue")

#
# b. Find the percentage of observations that fall within one standard deviation of the mean.

mean = mean(houses$price)
sd = sd(houses$price)
# percentage that falls within 1 sd
low = mean - 1/2*sd
high = mean + 1/2*sd
print(paste("Percent of data within within one standard deviation of the mean is ",sum(houses$price>=low & houses$price<=high)/total *100,"%", sep=""))


#
# c. Construct a boxplot.
boxplot(houses$price,
        main = "Box plot of the Selling Prices",
        xlab = "Houses",
        ylab = "Selling Prices",
        col = "blue",
        border = "orange",
        horizontal = FALSE,
        notch = TRUE)
