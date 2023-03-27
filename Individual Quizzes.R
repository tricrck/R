library(ggplot2)
library(dplyr)
library(scales)
library(ggridges)
library(ggpol)
library(ggbeeswarm)

setwd(choose.dir())

# read data
data <- read.csv("Telecom Churn Dataset Arun.csv", sep = ",")

## Quiz4
colnames(data)
# stacked bar chart
ggplot(data,
       aes(x = Gender,
           fill = InternetService)) +
  geom_bar(position = "stack")


# grouped bar plot
ggplot(data,
       aes(x = Gender,
           fill = InternetService)) +
  geom_bar(position = "dodge")

# grouped bar plot preserving zero count bars
ggplot(data,
       aes(x = Gender,
           fill = InternetService)) +
  geom_bar(position = position_dodge(preserve = "single"))

# bar plot, with each bar representing 100%
ggplot(data,
       aes(x = Gender,
           fill = InternetService)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion")

# bar plot, with each bar representing 100%,
# reordered bars, and better labels and colors
ggplot(data,
       aes(x = factor(Gender,
                      levels = c("Male", "Female")),
           fill = factor(InternetService,
                         levels = c("D", "F", "N"),
                         labels = c("DSL",
                                    "Fiber optic",
                                    "No")))) +
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",
       fill = "Payment Method",
       x = "Gender",
       title = "Payment Method by Gender") +
  theme_minimal()


data$Gender = factor(data$Gender,
                   levels = c("Male", "Female"))

# create a summary dataset

plotdata <- data %>%
  group_by(Gender, InternetService) %>%
  summarize(n = n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
plotdata


# create segmented bar chart
# adding labels to each segment
ggplot(plotdata,
       aes(x = factor(Gender,
                      levels = c("Male", "Female")),
           y = pct,
           fill = factor(InternetService,
                         levels = c("d", "f", "n"),
                         labels = c("DSL",
                                    "Fiber Optics",
                                    "NO")))) +
  geom_bar(stat = "identity",
           position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = percent) +
  geom_text(aes(label = lbl),
            size = 3,
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",
       fill = "Drive Train",
       x = "Class",
       title = "Automobile Drive by Class") +
  theme_minimal()
                   

# simple scatterplot
ggplot(data, 
       aes(x = TotalCharges, 
           y = MonthlyCharges)) +
  geom_point()

# enhanced scatter plot
ggplot(data, 
       aes(x = TotalCharges, 
           y = MonthlyCharges)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 90)) +
  scale_x_continuous(label = scales::dollar, 
                     limits = c(0, 8000)) + 
  labs(x = "Total Charges",
       y = "Monthly Charges",
       title = "Total Charges vs. Monthly Charges",
       subtitle = "Telecom Dataset Churn Prediction Charges comparison")

# scatterplot with linear fit line
ggplot(data, 
       aes(x = TotalCharges, 
           y = MonthlyCharges)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm")

# scatterplot with quadratic line of best fit
ggplot(data, 
       aes(x = TotalCharges, 
           y = MonthlyCharges)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

# scatterplot with loess smoothed line
ggplot(data, 
       aes(x = TotalCharges, 
           y = MonthlyCharges)) +
  geom_point(color= "steelblue") +
  geom_smooth(color = "tomato")

# scatterplot with loess smoothed line 
# and better labeling and color
ggplot(data, 
       aes(x = TotalCharges, 
           y = MonthlyCharges)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha = .6) +
  geom_smooth(size = 1.5,
              color = "darkgrey") +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(0, 90)) +
  scale_x_continuous(label = scales::dollar, 
                     limits = c(0, 8000)) + 
  labs(x = "Total Charges",
       y = "Monthly Charges",
       title = "Total Charges vs. Monthly Charges",
       subtitle = "Telecom Dataset Churn Prediction Charges comparison")
theme_minimal()


# simple line plot
ggplot(data,
       aes(x = Tenure, 
       y = MonthlyCharges)) +
  geom_line() 

# line plot with points
# and improved labeling
ggplot(data,
       aes(x = Tenure, 
           y = MonthlyCharges)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  labs(y = "Monthly Charges", 
       x = "Tenure",
       title = "Monthly Charges over Tenure",
       subtitle = "Telecom Dataset Churn Prediction",
       caption = "Source: https://www.kaggle.com/datasets/arunpothencode/telecom-dataset-churn-prediction/")

plotdata <- data %>%
  group_by(Churn) %>%
  summarize(mean_charges = mean(TotalCharges))

# plot mean salaries
ggplot(plotdata, 
       aes(x = Churn, 
           y = mean_charges)) +
  geom_bar(stat = "identity")

# plot mean salaries in a more attractive fashion

ggplot(plotdata, 
       aes(x = factor(Churn,
                      labels = c("No",
                                 "Yes")), 
           y = mean_charges)) +
  geom_bar(stat = "identity", 
           fill = "cornflowerblue") +
  geom_text(aes(label = dollar(mean_charges)), 
            vjust = -0.25) +
  scale_y_continuous(breaks = seq(0, 130000, 20000), 
                     label = dollar) +
  labs(title = "Mean Total Charges by Churn", 
       subtitle = "Telecom Dataset Churn Prediction",
       x = "Churn",
       y = "Mean Total Charges")

# plot the distribution of salaries 
# by rank using kernel density plots
ggplot(data, 
       aes(x = TotalCharges, 
           fill = Churn)) +
  geom_density(alpha = 0.4) +
  labs(title = "Total Charges distribution by Churn")

# plot the distribution of Total Charges distribution by Churn using boxplots
ggplot(data, 
       aes(x = Churn, 
           y = TotalCharges)) +
  geom_boxplot() +
  labs(title = "Total Charges distribution by Churn")

# plot the distribution of Total Charges distribution by Churn
ggplot(data, 
        aes(x = Churn, 
            y = TotalCharges)) +
  geom_boxplot(notch = TRUE, 
               fill = "cornflowerblue", 
               alpha = .7) +
  labs(title = "Total Charges distribution by Churn")

# plot the distribution of salaries 
# by Churn using violin plots
ggplot(data, 
       aes(x = Churn,
           y = TotalCharges)) +
  geom_violin() +
  labs(title = "Total Charges distribution by Churn")

# plot the distribution using violin and boxplots
ggplot(data, 
       aes(x = Churn, 
           y = TotalCharges)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Total Charges distribution by Churn")

# create ridgeline graph
ggplot(data, 
       aes(x = Churn, 
           y = TotalCharges, 
           fill = TotalCharges))+
  geom_density_ridges() + 
  theme_ridges() +
  labs("Contract by Tenure") +
  theme(legend.position = "none")

# calculate means, standard deviations,
# standard errors, and 95% confidence 
# intervals by Churn
plotdata <- data %>%
  group_by(Churn) %>%
  summarize(n = n(),
            mean = mean(TotalCharges),
            sd = sd(TotalCharges),
            se = sd / sqrt(n),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
# plot the means and standard errors
ggplot(plotdata, 
       aes(x = Churn, 
           y = mean, 
           group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1)

# calculate means and standard errors by Churn and Gender
plotdata <- data %>%
  group_by(Churn, Gender) %>%
  summarize(n = n(),
            mean = mean(TotalCharges),
            sd = sd(TotalCharges),
            se = sd/sqrt(n))

# plot the means and standard errors by sex
ggplot(plotdata, aes(x = Churn,
                     y = mean, 
                     group=Gender, 
                     color=Gender)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin  =mean - se, 
                    ymax = mean+se), 
                width = .1)

# plot the means and standard errors by Gender (dodged)
pd <- position_dodge(0.2)
ggplot(plotdata, 
       aes(x = Churn, 
           y = mean, 
           group=Gender, 
           color=Gender)) +
  geom_point(position = pd, 
             size = 3) +
  geom_line(position = pd,
            size = 1) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1, 
                position= pd)

# improved means/standard error plot
pd <- position_dodge(0.2)
ggplot(plotdata, 
       aes(x = factor(Churn, 
                      labels = c("No",
                                 "Yes")), 
           y = mean, 
           group=Gender, 
           color=Gender)) +
  geom_point(position=pd, 
             size = 3) +
  geom_line(position = pd, 
            size = 1) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1, 
                position = pd, 
                size = 1) +
  scale_y_continuous(label = scales::dollar) +
  scale_color_brewer(palette="Set1") +
  theme_minimal() +
  labs(title = "Mean Total Charges by Churn and Gender",
       subtitle = "(mean +/- standard error)",
       x = "", 
       y = "",
       color = "Gender")

# plot the distribution of salaries 
# by rank using strip plots
ggplot(data, 
       aes(y = Churn, 
           x = TotalCharges)) +
  geom_point() + 
  labs(title = "Total Charges distribution by Churn")

# plot the distribution of salaries
# by rank using jittering
ggplot(data, 
       aes(y = Churn, 
           x = TotalCharges)) +
  geom_jitter() + 
  labs(title = "Total Charges distribution by Churn")
# plot the distribution of Total Charges 
# by rank using jittering
ggplot(data, 
       aes(x = factor(Churn, 
                      labels = c("No",
                                 "Yes")), 
           y = TotalCharges, 
           color = Churn)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3) +
  geom_jitter(alpha = 0.5, 
              width=.2) + 
  scale_y_continuous(label = dollar) +
  labs(title = "Telecom Dataset TotalCharges by Churn", 
       subtitle = "Telecom Dataset Churn Prediction",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()
# plot the distribution of salaries 
# by rank using jittering
ggplot(data, 
       aes(x = factor(Churn, 
                      labels = c("No",
                                 "Yes")), 
           y = salary, 
           fill=rank)) +
  geom_boxjitter(color="black",
                 jitter.color = "darkgrey",
                 errorbar.draw = TRUE) +
  scale_y_continuous(label = dollar) +
  labs(title = "Telecom Dataset TotalCharges by Churn", 
       subtitle = "Telecom Dataset Churn Prediction",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")

# plot the distribution of salaries 
# by rank using beewarm-syle plots

ggplot(data, 
       aes(x = factor(Churn, 
                      labels = c("No",
                                 "Yes")), 
           y = salary, 
           color = rank)) +
  geom_quasirandom(alpha = 0.7,
                   size = 1.5) + 
  scale_y_continuous(label = dollar) +
  labs(title = "Telecom Dataset TotalCharges by Churn", 
       subtitle = "Telecom Dataset Churn Prediction",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none")

data(gapminder, package="gapminder")

# subset Electronic check PaymentMethods in who Churn.

plotdata <- data %>%
  filter(PaymentMethod == "Electronic check" & 
           Churn == "Yes")

# basic Cleveland plot of Tenure by InternetService
ggplot(plotdata, 
       aes(x= Tenure, y = InternetService)) +
  geom_point()

# Sorted Cleveland plot
ggplot(plotdata, 
       aes(x=Tenure, 
           y=reorder(InternetService, Tenure))) +
  geom_point()

# Fancy Cleveland plot
ggplot(plotdata, 
       aes(x=Tenure, 
           y=reorder(InternetService, Tenure))) +
  geom_point(color="blue", 
             size = 2) +
  geom_segment(aes(x = 40, 
                   xend = Tenure, 
                   y = reorder(InternetService, Tenure), 
                   yend = reorder(InternetService, Tenure)),
               color = "lightgrey") +
  labs (x = "Tenure",
        y = "",
        title = "Tenure by Internet Service",
        subtitle = "Telecom Dataset Churn Prediction") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
