library(ggplot2)
library(dplyr)
fifa21 = read.csv(file = "fifa21.csv")
head(fifa21, 10)

# Is the pace rating of more players greater than their physicality rating?
fifa21 <- fifa21 %>%
  mutate(SpeedVsStrength = if_else(sprint_speed> strength, 1, 0))

sum(fifa21$SpeedVsStrength)/nrow(fifa21)

# format and remove null data
fifa21$value <- as.double(gsub('[.]', '', fifa21$value))
fifa21 <- filter(fifa21, !is.na(value))

# summary of data
fifa21.summary <- fifa21 %>%
  select(height, weight, value, wage, agility, vision, stamina, balance, sprint_speed, dribbling, shot_power, weak_foot, skill_moves) %>%
  summarise_each(funs(min = min, 
                      q25 = quantile(., 0.25, na.rm = TRUE), 
                      median = median, 
                      q75 = quantile(., 0.75, na.rm = TRUE), 
                      max = max,
                      mean = mean))
print(fifa21.summary)


# What is the relatinship of Speed and Strength to Value:
# strength
ggplot() + 
  geom_smooth(data = fifa21, mapping = aes(strength, value), color = 'green') + 
  geom_smooth(data = fifa21, mapping = aes(sprint_speed, value), color = 'red') +
  xlab('Physical parameters') + ylab('Value') + ggtitle('Relatinship of Speed and Strength to Value')


boxplot(fifa21[,15:48], las=2, main= "Important Field Attributes to a Higher Value Box Plot", ylab= "Attribute Value")
