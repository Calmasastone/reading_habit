# Portfolio #2 (in process)

library(tidyverse)
library(janitor)

rawdf <- read_csv("C:/Users/anzhu/OneDrive/Документы/R/Studies/reading_habit.csv")

colnames(rawdf)
summary(rawdf) # statistical info
glimpse(rawdf) # colnames, types

# str(rawdf) - less useful compare to summary

colSums(is.na(rawdf)) # checking for NA values

df <- na.omit(rawdf) # removing NA values

df <- clean_names(df) # making columns clear

colnames(df)

df %>% 
  group_by(sex) %>% 
  summarise(mean_age = mean(age))

View(sex_summary <- df %>% 
  group_by(sex) %>% 
  summarize(total = n()) %>% # count
  arrange(-total)) # sort by desc

# Sex distribution
df %>% 
  group_by(sex) %>% 
  summarise(total = n()) %>% 
  arrange(-total) %>% 
  mutate(percentage = total / sum(total) * 100)

# Race distribution
df %>% 
  group_by(race) %>% 
  summarise(total = n()) %>% 
  arrange(-total) %>% 
  mutate(percentage = round((total / sum(total)) * 100, 1))

# marriage distribution 

df %>% 
  group_by(marital_status) %>% 
  summarise(total = n()) %>% 
  arrange(-total) %>% 
  mutate(percentage = round((total / sum(total)) * 100, 1))

# showing Sex distribution

ggplot(data = sex_summary, aes(x = sex, y = total, fill = sex)) +
  geom_bar(stat = "identity") + # 
  geom_text(aes(label = total), position = position_stack(vjust = 1.05),
            fontface = "bold") +
  labs(title = "Distribution of Sex", x = "Sex", y = "Total") +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "blue")) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"))


