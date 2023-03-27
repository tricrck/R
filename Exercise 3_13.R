income = c(152, 144, 162, 154, 146, 241, 127, 141, 171, 177, 138, 132, 192)

#a The 90th percentile.

quantile(income, 0.90)

#b The median.

median(income)

#c The first quartile.

quantile(income, probs = 0.25)

#d The third quartile.

quantile(income, 0.75)

#e The 10th percentile.

quantile(income, 0.1)

#f, The interquartile range.

Q1 <- quantile(income, 0.25)
Q3 <- quantile(income, 0.75)

# Calculate the IQR
IQR <- Q3 - Q1
IQR

#g Develop a graphical display of a five-number summary and a box-and-whiskers display.

# Five Number Summary of the Income Data 

summary(income)

# Create a box-and-whiskers plot
boxplot(income, main = "Internists income Box-and-Whiskers Plot", 
        xlab = "internists Income", ylab = "income Value")
