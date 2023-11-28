# Reading habit (Early access - in progress)

### [ASKING STAGE] Questions for the project:

# 1. What is the distribution of dataset in terms of: age, sex, race, etc.
# 2. Do people with higher education tend to have higher income?
# 3. Find the trends between how many books do people usually read depends on their: age, sex, race, marital status, etc.
# 4. What kind of books do people prefer? e-book, printed, or audio books?
# 5. Is there any correlation between people’s employment and reading? 
# 6. Do employed individuals read more or less compared to unemployment ones?

### [PREPARE DATA] Load packages, get to know with data

pacman::p_load(tidyverse, janitor)

df <- read_csv('C:/Users/anzhu/OneDrive/Документы/R/Studies/reading_habit.csv')

head(df)
dim(df)
summary(df) 
colnames(df)
lapply(df, unique) # checking for unique values, finding mistakes in data

### [PROCESS STAGE] Getting rid of NA, possible mistakes in df, e.t.c.

df <- clean_names(df) # Removing unnecessary space and upper case from cols 
colnames(df)

# Shorten col names for the convenience

df <- df %>% 
  rename(
  read_books_12mon = "how_many_books_did_you_read_during_last_12months",
  read_p_books_12mon = "read_any_printed_books_during_last_12months",
  read_e_books_12mon = "read_any_e_books_during_last_12months",
  read_a_books_12mon = "read_any_audiobooks_during_last_12months",
  do_you_read_news_newspapers = "do_you_happen_to_read_any_daily_news_or_newspapers",
  do_you_read_magazines_journals = "do_you_happen_to_read_any_magazines_or_journals")

colnames(df)

lapply(df, unique)
colSums(is.na(df))[colSums(is.na(df)) > 0] # Time to spot cols with NA values
sum(is.na(df)) # Total NA is 1.560 

# I prefer to write a function which shortens the following process 'smart_count' shows counted responses for specific columns

smart_count <- function(data, col_name) {
  result <- data %>%
    group_by(across({{col_name}})) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  return(result)
}

# Our cols with NA values

smart_count(df, 'read_p_books_12mon')
smart_count(df, 'read_a_books_12mon')
smart_count(df, 'read_e_books_12mon')
smart_count(df, 'last_book_you_read_you')

# Pulling NA into "Don't know"

df$read_p_books_12mon[is.na(df$read_p_books_12mon)] <- "Don’t know"
df$read_e_books_12mon[is.na(df$read_e_books_12mon)] <- "Don’t know"
df$read_a_books_12mon[is.na(df$read_a_books_12mon)] <- "Don’t know"

# Uniting 2 similar responses into one (there's an error '>9<$100,000')

df$incomes <- ifelse(
  df$incomes %in% c('9$100,000 to under $150,000'), 
  '$100,000 to under $150,000', df$incomes)

# Creating "Don't know" response for wrong answers '8', '9', and NA values

smart_count(df, 'last_book_you_read_you') # last book you read responses

df$last_book_you_read_you <- ifelse(
  df$last_book_you_read_you %in% c(NA, '8', '9',''),
  "Don’t know",
  df$last_book_you_read_you)

# Making sure everything is in order and I got rid of mistakes

colSums(is.na(df))[colSums(is.na(df)) > 0]
lapply(df, unique)

# I've just processed the data (cleaned NA values, fixed mistakes).

### [ANALYSIS STAGE]

# 1. What is the distribution of dataset in terms of: age, sex, race, etc.

colnames(df)
summary(df)

# Age distribution

sd(df$age)
quantile(df$age, 0.25)
quantile(df$age, 0.75)
IQR(df$age) # 50% of the survey population lies between 32 and 62 years old

par(mfrow = c(2, 1)) # 2 rows & 1 col for the graph

hist(df$age[df$sex == 'Male'],
     xlim = c(0, 100),
     main = 'Age Distribution for Males',
     xlab = 'Age',
     col = 'blue2')

hist(df$age[df$sex == 'Female'],
     xlim = c(0, 100),
     main = 'Age Distribution for Females',
     xlab = 'Age',
     col = 'pink2')

par(mfrow = c(1, 2))

boxplot(df$age[df$sex == 'Male'],
        ylim = c(0, 100),
        main = 'Age Distribution for Males',
        ylab = 'Age',
        col = 'blue2')

boxplot(df$age[df$sex == 'Female'],
        ylim = c(0, 100),
        main = 'Age Distribution for Females',
        ylab = 'Age',
        col = 'pink2')

par(mfrow = c(1, 1)) # clear parameter

## {Age & Sex Conclusion}: The age of men participating in the survey is lower than women

# Race Distribution

df %>%
  group_by(race) %>%
  summarise(n = n()) %>%
  mutate(race = factor(race, levels = race[order(n)])) %>%
  ggplot(aes(x = n, y = race)) +
  geom_bar(stat = "identity", fill = 'blue2') +
  labs(title = 'Race Distribution', xlab = 'Total Number', ylab = 'Race') +
  theme_minimal()

## {Race Conclusion}: we see that white and black or african american races are the most frequently participating races in this survey.

df %>% 
  group_by(marital_status) %>% 
  summarise(n = n()) %>% 
  mutate(marital_status = factor(marital_status, 
                                 levels = marital_status[order(n)])) %>% 
  ggplot(aes(x = n, y = marital_status)) +
  geom_bar(stat = 'identity', fill = 'blue2') +
  labs(title = 'Marital Status Distribution', x = 'Total Number', y = 'Type') +
  theme_minimal()

# To be continued...