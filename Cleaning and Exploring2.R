install.packages("readxl")

library("readxl")
data<- read_excel("EMSI_JobChange_UK.xlsx")

head(data)

str(data)

df_status(data)

unique(data$Industry)

# Average change of jobs per city
data %>% 
  group_by(City) %>%   # group by city
  summarise(Average_Change  = mean(`Change`)) 

# Average change of jobs per industry
data %>%                    
  group_by(Industry) %>%   # group by Industry
  summarise(Average_Change  = mean(Change)) 

# Negative change of jobs per city
data %>% 
  filter(Change<0) %>%
  group_by(City) %>%
  summarise(Average_Change  = mean(Change)) %>%  
  arrange(desc(Average_Change)) 

# Negative change of jobs per industry
data %>% 
  filter(Change<0) %>%
  group_by(Industry) %>%
  summarise(Average_Change  = mean(Change)) %>%  
  arrange(desc(Average_Change))   

# Positive change of jobs per city
data %>% 
  filter(Change>0) %>%
  group_by(City) %>%
  summarise(Average_Change  = mean(Change)) %>%  
  arrange(desc(Average_Change)) 

# Positive change of jobs per industry
data %>% 
  filter(Change>0) %>%
  group_by(Industry) %>%
  summarise(Average_Change  = mean(Change)) %>%  
  arrange(desc(Average_Change)) 

data %>% 
  filter(str_detect(string = Industry, 
                      pattern = "HUMAN")) %>% 
  {unique(.$Industry)}

