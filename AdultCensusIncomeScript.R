# Install and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")


# Set ggplot theme
theme_set(theme_minimal())

# Read raw data from Github repository
raw_data <- read.csv("https://raw.githubusercontent.com/MrKempe/AdultCensusIncome/master/adult.csv", stringsAsFactors=TRUE)

# Replace '?' with NA
clean_data <- na_if(raw_data, "?") %>% 
  mutate_if(is.factor, fct_explicit_na) %>% 
  droplevels()
  
# Convert the Income column to a factor 
clean_data <- clean_data %>% 
  mutate(income = as.factor(ifelse(income=="<=50K",0,1))) 

# Create train and validation set
set.seed(1, sample.kind="Rounding")
validation_index <- createDataPartition(y = clean_data$income, times = 1, p = 0.3, list = FALSE)
training_set <- clean_data[-validation_index,]
validation_set <- clean_data[validation_index,]

avg_income <- mean(as.numeric(as.character(training_set$income)))
avg_income

age_graph <- training_set %>% 
  group_by(age, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  complete(income, fill = list(n = 0, pct = 0)) %>% 
  group_by(age) %>% 
  mutate(max = max(as.numeric(income))) %>% 
  filter(as.numeric(income)==max) %>% 
  ggplot(aes(x = age, y = pct)) +
    geom_line(size = 1) +
    stat_smooth(geom = "area", method = "loess", span = 1/3, alpha = 1/2, fill = "#9ecae1") +
    scale_y_continuous(name = "Percentage", limits = c(0,0.45), labels = percent) +
    scale_x_continuous(name = "Age", breaks = seq(20,90,10), labels = seq(20,90,10))
age_graph

workclass_graph <- training_set %>% 
  group_by(workclass, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(workclass, order), y = pct, fill = income)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(name = "Workclass") +
    scale_y_continuous(name= "Percentage", labels = percent) +
    scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
    geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
    coord_flip()
workclass_graph

education_graph <- training_set %>% 
  group_by(education, education.num, income) %>% 
  summarize(n=n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0, NA, n/sum(n))) %>% 
  ggplot(aes(x = reorder(education, education.num), y = pct, fill = income)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(name = "Education") +
    scale_y_continuous(name = "Percentage", labels = percent) +
    scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") +
    geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
    coord_flip()
education_graph

marital_status_graph <- training_set %>% 
  group_by(marital.status, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(marital.status, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Marital Status") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
marital_status_graph

occupation_graph <- training_set %>% 
  group_by(occupation, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(occupation, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Occupation") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
occupation_graph

relationship_graph <- training_set %>% 
  group_by(relationship, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(relationship, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Relationship") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
relationship_graph

race_graph <- training_set %>% 
  group_by(race, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(race, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Race") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
race_graph

sex_graph <- training_set %>% 
  group_by(sex, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(sex, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Sex") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
sex_graph

hours_per_week_graph <- training_set %>% 
  group_by(hours.per.week, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  complete(income, fill = list(n = 0, pct = 0)) %>% 
  group_by(hours.per.week) %>% 
  mutate(max = max(as.numeric(income))) %>% 
  filter(as.numeric(income)==max) %>% 
  ggplot(aes(x = hours.per.week, y = pct)) +
  geom_line(size = 1) +
  stat_smooth(geom = "area", method = "loess", span = 1/3, alpha = 1/2, fill = "#9ecae1") +
  scale_y_continuous(name = "Percentage", limits = c(0,0.45), labels = percent) +
  scale_x_continuous(name = "Hours per Week", breaks = seq(20,90,10), labels = seq(20,90,10))
hours_per_week_graph

native_country_graph <- training_set %>% 
  group_by(native.country, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(native.country, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Native Country") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
native_country_graph

ggplot(training_set, aes(native.country)) + geom_bar() + coord_flip()
