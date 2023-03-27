library(ggplot2)
library(ggcorrplot)
library(ggplot2)
library(visreg)
library(survival)
library(survminer)
library(readr)
library(vcd)
library(survival)
library(survminer)
require(reshape2)


setwd(choose.dir())

# read data
df <- read.csv("Telecom Churn Dataset Arun.csv", sep = ",")

## Quiz7

# select numeric variables
df1 <- dplyr::select_if(df, is.numeric)

# calulate the correlations
r <- cor(df1, use="complete.obs")
round(r,2)

ggcorrplot(r)

ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


TotalCharges_lm <- lm(TotalCharges ~ MonthlyCharges + Tenure, 
                data = df1)

# conditional plot of Total Charges vs. Tenure

visreg(TotalCharges_lm, "Tenure", gg = TRUE) 


# conditional plot of Total Charges vs. Monthly Charges
visreg(TotalCharges_lm, "MonthlyCharges", gg = TRUE) +
  scale_y_continuous(label = scales::dollar) +
  scale_x_continuous(label = scales::dollar) +
  labs(title = "Relationship between Total Charges vs. Monthly Charges",
       subtitle = "conditional plot of Total Charges vs. Monthly Charges",
       caption = "source: Telecom Dataset Churn Prediction",
       y = "Total Charges",
       x = "MonthlyCharges")

# fit logistic model for predicting
# marital status: married/single
df$Married <- factor(df$Married,
                        labels = c("Yes",
                                  "No"))
df$Gender <- factor(df$Married,
                     labels = c("Male",
                                "Female"))
df$PhoneService <- factor(df$PhoneService,
                     labels = c("Yes",
                                "No"))
df$InternetService <- factor(df$InternetService,
                                   labels = c("DSL",
                                              "Fiber Optics",
                                              "NO"))

df_glm <- glm(Married ~ Gender + TotalCharges + InternetService, 
              family="binomial", 
              data=df)

# plot results

visreg(df_glm, "InternetService", 
       gg = TRUE, 
       scale="response") +
  labs(y = "Prob(Married)", 
       x = "InternetService",
       title = "Relationship of Tenure and Married",
       subtitle = "controlling for Gender, PhoneService, and InternetService",
       caption = "source: Telecom Churn Dataset")


# plot results

visreg(df_glm, "InternetService",
       by = "Gender",
       gg = TRUE, 
       scale="response") +
  labs(y = "Prob(Married)", 
       x = "Age",
       title = "Relationship of Tenure and Married",
       subtitle = "controlling for Gender, PhoneService, and InternetService",
       caption = "source: Telecom Churn Dataset")

# plot survival curve
df$Churn <- factor(df$Churn,
                          labels = c("Yes",
                                     "No"))
##df <- data.frame(df$Churn==df$Churn, df$SeniorCitizen==df$SeniorCitizen)
length(df$Churn) <- length(df$SeniorCitizen)
sfit <- survfit(Surv(SeniorCitizen, Churn) ~  1, data=df)
ggsurvplot(sfit,
           title="df$SeniorCitizen curve for Churn survival") 

# plot survival curve for men and women
sfit <- survfit(Surv(MonthlyCharges, Churn) ~  Gender, data=df)
ggsurvplot(sfit, 
           conf.int=TRUE, 
           pval=TRUE,
           legend.labs=c("Male", "Female"), 
           legend.title="Gender",  
           palette=c("cornflowerblue", "indianred3"), 
           title="MonthlyCharges Curve for Churn survival",
           xlab = "Gender")


# create a table
tbl <- xtabs(~Contract + Married + Gender, df)
ftable(tbl)

# create a mosaic plot from the table

mosaic(tbl, main = "Telecom Churn Dataset")

mosaic(tbl, 
       shade = TRUE,
       legend = TRUE,
       labeling_args = list(set_varnames = c(Gender = "Gender",
                                             Survived = "Survived",
                                             Class = "Telecom Churn")),
       set_labels = list(Survived = c("No", "Yes"),
                         Class = c("1st", "2nd", "3rd", "Crew"),
                         Sex = c("F", "M")),
       main = "Telecom Churn Dataset")

