# 1. The student directory for a large university has 400 pages with 130 names per page, 
    #a total of 52,000 names. Using software, show how to select a simple random sample of 10 names.

 # Randomly generate strings to represent the student names in the dataframe

# Define a vector of characters to sample from
chars <- c(letters, LETTERS, 0:9)

# Generate 15,000 random strings
rand_strings <- replicate(52000, paste0(sample(chars, 10, replace=TRUE), collapse=""))


# name the generated student directory df.
df <- data.frame(matrix(unlist(rand_strings), nrow=length(rand_strings), byrow=TRUE))
colnames(df) <- c("StudentNames")
head(df, 10)


### Using software, show how to select a simple random sample of 10 names.

df[sample(nrow(df), 10), ]



# 2.From the Murder data file, use the variable murder, which is the murder rate (per 100,000 population) 
      # for each state in the U.S. in 2017 according to the FBI Uniform Crime Reports. 
      # At first, do not use the observation for D.C. (DC). Using software:
murder <- read.table("https://stat4ds.rwth-aachen.de/data/Murder.dat", 
                      header=TRUE)

# remove DC from observation which is observation number 51
murder1 <- murder[-c(51), ]

# a. Find the mean and standard deviation and interpret their values.

mean(murder1$murder)

sd(murder1$murder)
#
#
## Based on the FBI Uniform Crime Reports for 2017, we found that the average murder rate 
#in the United States was 4.874, and this value represents the center of the 
#distribution of murder rates. The standard deviation of murder rates in 2017 was 2.586291, 
#indicating that the data points are relatively close to the mean.



### b. Find the five-number summary, and construct the corresponding boxplot.
# Five summary with R's fivenum()
fivenum(murder1$murder)

# construct the corresponding boxplot

boxplot(murder1$murder,horizontal=TRUE)
text(x=fivenum(murder1$murder),labels=fivenum(murder1$murder),y=1.3)

## From the mean and standard deviation we observed that the mean murder is 4.856

# c. Now include the observation for D.C. What is affected more by this outlier: The mean or the median? 
# mean deviation
# use murder dataframe as it included D.C
mean(murder$murder) - mean(murder1$murder)
# median deviation
median(murder$murder) - median(murder1$murder)


## The mean is affected more by this outlier compared to the median


## 3.The Houses data file lists the selling price (thousands of dollars), size (square feet), tax bill (dollars)
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
total = sum(houses$price)
low = mean - 1/2*sd
high = mean + 1/2*sd
print(paste("Percent of one standard deviation of the mean is ",sum(houses$price>=low & houses$price<=high)/total *100,"%", sep=""))


#
# c. Construct a boxplot.
boxplot(houses$price,
        main = "Box plot of the Selling Prices",
        xlab = "Houses",
        ylab = "Selling Prices",
        col = "black",
        border = "blue",
        horizontal = FALSE,
        notch = TRUE)
