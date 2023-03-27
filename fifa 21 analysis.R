##
# FIFA Visualization and Analysis.
##
library(ggplot2)
library(corrplot)
library(dplyr)
library(wordcloud)
library(tm)
library(SnowballC)

fifa21 = read.csv(file = "fifa21.csv")
head(fifa21, 10)


# format and remove null data for the player value
fifa21$value <- as.double(gsub('[.]', '', fifa21$value))
fifa21 <- filter(fifa21, !is.na(value))

# Correlation Matrix Visualization of "weak_foot","skill_moves", "value","wage","ball_control", "dribbling", "marking", "slide_tackle", "stand_tackle", "aggression", "reactions", and "att_position".

fifacor <- fifa21[, c(11:22)]
corrplot(cor(fifacor),method = 'circle', order = 'AOE', addCoef.col = 'black',
         cl.pos = 'n', col = COL2('BrBG'), title = "Correlation Matrix Visualization of 11 attributes of players")


# What is the relationship of Ball Control and Reactions to Value:
ggplot() + 
  geom_smooth(data = fifa21, mapping = aes(reactions, value), color = 'green') + 
  geom_smooth(data = fifa21, mapping = aes(ball_control, value), color = 'red') +
  xlab(" Percentage ")+
  scale_y_continuous(name = "Player Value")+
  labs(title="Relatinship of Ball Control and Reactions to Player Value.", 
       subtitle = "Ball Control is in Red and Reactions is on green",
       caption="Source: Kaggle Data Sets")

# Scatter plot of Wage to their Value Based of Work Rate.

ggplot(data = fifa21, aes(x = value, y = wage, color = work_rate)) + 
  geom_point()+
  scale_x_continuous(name = "Player Value")+
  labs(title="Scatter plot of Wage to their Value Based of Work Rate.", 
       subtitle = "Relationship Between The Player Wage and Their Value",
       caption="Source: Kaggle Data Sets")



#WordCloud Of preffered positions
wordcloud(words = fifa21$preferred_positions, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))


# Current Player Rating of all the Fifa21 in relation to Work Rate

ggplot(data = fifa21,                 
       aes(x = current_rating,           # The x axis aesthetic is current rating
           fill = work_rate)      # The fill aesthetic is work rate
) +                    # Add in more features.
  geom_density(alpha = 0.2) + # Add a density based on the aesthetics.
  theme_bw(base_size = 24) +  # black and white background with big font
  labs(fill = "") +           # no label for the fill variable above the key
  xlab("Current Player Rating") +    # label for x axis
  ylab ("Proportion of all Players") +                 # label for y axis
  theme(axis.title.y = element_text(vjust=1.5)) +
  ggtitle ("Current Player Rating of all the Fifa21 in relation to Work Rate") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
          



