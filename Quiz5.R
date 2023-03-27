#Loading Packages
library(tidyverse)
library(ggplot2)
library(scales)
library(ggridges)


setwd(choose.dir())

# read data
df <- read.csv("Telecom Churn Dataset Arun.csv", sep = ",")

## Quiz5

# plot Tenure vs. Total Charges
ggplot(df,
       aes(x = Tenure,
           y = TotalCharges)) +
  geom_point() +
  labs(title = "Total Charges by Tenure")

# plot Tenure vs. Total Charges (color represents Churn)
ggplot(df,
       aes(x = Tenure,
           y = TotalCharges,
                     color=Churn)) +
  geom_point() +
  labs(title = "Total Charges by Churn and Tenure")

# plot Tenure vs. Total Charges
# (color represents Churn, shape represents Gender)
ggplot(df,
       aes(x = Tenure,
           y = TotalCharges,
           color=Churn,
           shape = Gender)) +
  geom_point(size = 3,
             alpha = .6) +
  labs(title = "Total Charges by Churn, Gender, and Tenure")

# plot Tenure vs. Total Charges
# (color represents Churn and size represents Gender)
ggplot(df,
       aes(x = Tenure,
           y = TotalCharges,
           color=Churn,
           size = Gender)) +
  geom_point(alpha = .6) +
  labs(title = "Total Charges by Churn, Gender, and Tenure")

# plot Tenure vs. Total Charges with
# fit lines (color represents Gender)
ggplot(df,
       aes(x = Tenure,
           y = TotalCharges,
           color=Gender)) +
  geom_point(alpha = .4,
             size = 3) +
  geom_smooth(se=FALSE,
              method = "lm",
              formula = y~poly(x,2),
              size = 1.5) +
  labs(x = "Tenure.",
       title = "Total Charges by Tenure, Gender",
       subtitle = "Telecom Dataset Churn Prediction",
       y = "",
       color = "Sex") +
  scale_y_continuous(label = scales::dollar) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# plot Total Charges histograms by Churn
ggplot(data, aes(x = TotalCharges)) +
  geom_histogram(fill = "cornflowerblue",
                 color = "white") +
  facet_wrap(~Churn, ncol = 1) +
  labs(title = "Total Charges histograms by Churn")

# plot Total Charges histograms by Churn and Gender
ggplot(data, aes(x = TotalCharges / 1000)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue") +
  facet_grid(Gender ~ Churn) +
  labs(title = "Total Charges histograms by Gender and Churn",
       x = "Total Charges ($1000)")

# calculate means and standard erroes by Gender,
# Churn and Internet Service


plotdata <- df %>%
  group_by(Gender, Churn, InternetService) %>%
  summarize(n = n(),
            mean = mean(TotalCharges),
            sd = sd(TotalCharges),
            se = sd / sqrt(n))

# create better labels for Internet Service
plotdata$InternetService <- factor(plotdata$InternetService,
                              labels = c("DSL",
                                         "Fiber Optics",
                                         "NO"))
# create plot
ggplot(plotdata, 
       aes(x = Gender, 
           y = mean,
           color = Gender)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                width = .1) +
  scale_y_continuous(breaks = seq(1500, 9000, 1500),
                     label = scales::dollar) +
  facet_grid(. ~ Churn + InternetService) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(x="", 
       y="", 
       title="TotalCharges by gender, InternetService, and Churn",
       subtitle = "(Means and standard errors)") +
  scale_color_brewer(palette="Set1")

# plot Tenure by Total Charges separately 
# for those who had churn

# Select the Churn is Yes data
plotdata <- dplyr::filter(df, 
                          Churn == "Yes")

# plot Tenure, for those who have churned
ggplot(plotdata, aes(x=Tenure, y = TotalCharges)) +
  geom_line(color="grey") +
  geom_point(color="blue") +
  facet_wrap(~InternetService) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  labs(title = "Tenure and InternetService for those who have churned",
       x = "Total Charges",
       y = "Tenure") 

