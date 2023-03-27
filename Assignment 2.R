## Question 1

# a) Based on a descriptive graphic, describe the shape of the sample data distribution. Find and interpret point estimates of the population mean and standard deviation.
# Load the Chicago data
chicago_data <- read.table("https://stat4ds.rwth-aachen.de/data/Chicago.dat", header = TRUE)

# Create a histogram to describe the shape of the data distribution
hist(chicago_data$income)

#Based on the histogram of the annual income distribution for heads of households 
#  living in public housing in Chicago, the distribution appears to be slightly 
#  skewed to the right. The majority of the incomes are clustered towards the lower end of 
#  the distribution, with a long tail extending to the higher end of the distribution. 
#  This is indicative of a right-skewed distribution.

# Calculate point estimates of the population mean and standard deviation

# mean
mean(chicago_data$income)

# standard deviation
sd(chicago_data$income)

# b) Construct a 95% confidence interval for μ, using R software.

# Construct a 95% confidence interval for μ using t.test
t.test(chicago_data$income, conf.level = 0.95)$conf.int

# Question 2
# a. Conduct a descriptive statistical analysis using graphs and numerical summaries.
# Load the Anorexia data
anorexia_data <- read.table("https://stat4ds.rwth-aachen.de/data/Anorexia.dat", header = TRUE)

# Filter data for the 17 girls who received family therapy
table(anorexia_data$therapy)

family_therapy_data <- subset(anorexia_data, therapy == "f")

# Descriptive statistical analysis
summary(family_therapy_data$before)
summary(family_therapy_data$after)

# Create a boxplot to visualize the distribution of weights before and after therapy
boxplot(family_therapy_data$before, 
        family_therapy_data$after, 
        main = "Distribution of Weights Before and After Therapy", 
        ylab = "Weight (in lbs)", 
        names = c("Before", "After"))

# Create a histogram to visualize the distribution of weights before and after therapy
par(mfrow=c(1,2))
hist(family_therapy_data$before,
     main = "Distribution of Weights Before Therapy", 
     xlab = "Weight (in lbs)")
hist(family_therapy_data$after, 
     main = "Distribution of Weights After Therapy", 
     xlab = "Weight (in lbs)")

# b) Construct a 95% confidence interval for the difference between the population mean weight changes for the family therapy and the control.

# Filter data for the control group
control_data <- subset(anorexia_data, therapy == "c")

# Conduct a t-test to compare the mean weight changes for family therapy and control
t.test(family_therapy_data$after - family_therapy_data$before, control_data$after - control_data$before, conf.level = 0.95)


#The 95% confidence interval for the difference in means is between 2.98 and 12.45, 
# which means we can be 95% confident that the true difference in population means lies between these two values. 
# This interval does not include zero, which provides further evidence against the null hypothesis.
