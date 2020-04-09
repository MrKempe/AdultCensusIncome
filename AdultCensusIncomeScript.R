# Install and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("knitr", repos = "http://cran.us.r-project.org")


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

# Exploratory Data Analysis
str(raw_data)
colnames(raw_data)

avg_income <- mean(as.numeric(as.character(training_set$income)))
avg_income

#################################
### EXPLORATORY DATA ANALYSIS ###
#################################

#################################
########### EDA - AGE ###########
#################################

age_graph_distr <- training_set %>% 
  ggplot(aes(x=age)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(name = "Age", breaks = seq(10,90,10), labels = seq(10,90,10)) +
  scale_y_continuous(name = "Count", breaks = seq(0, 700, 100), labels = seq(0,700,100)) +
  ggtitle("Histogram of Age") +
  theme(plot.title = element_text(hjust = 0.5))

age_graph_pct <- training_set %>% 
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
    scale_y_continuous(name = "", limits = c(0,0.45), labels = percent) +
    scale_x_continuous(name = "Age", breaks = seq(20,90,10), labels = seq(20,90,10)) +
    ggtitle("Income > 50K by Age") +
    theme(plot.title = element_text(hjust = 0.5))

grid.arrange(age_graph_distr, age_graph_pct, nrow = 1)

#################################
######## EDA - WORKCLASS ########
#################################

workclass_graph_distr <- training_set %>% 
  group_by(workclass, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(workclass, order), y = n)) +
  scale_x_discrete(name = "Workclass") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of Workclass") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

workclass_graph_pct <- training_set %>% 
  group_by(workclass, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(workclass, order), y = pct, fill = income)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(name = "Workclass") +
    scale_y_continuous(name= "Percentage", labels = percent) +
    scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
    geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
    ggtitle("Income > 50K by Workclass") +
    theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
    coord_flip()
grid.arrange(workclass_graph_distr, workclass_graph_pct, nrow = 1)

#################################
######## EDA - EDUCATION ########
#################################

education_graph_distr <- training_set %>% 
  group_by(education, education.num, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n))) %>% 
  ggplot(aes(x = reorder(education, education.num), y = n)) +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of Education") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

education_graph_pct <- training_set %>% 
  group_by(education, education.num, income) %>% 
  summarize(n=n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0, NA, n/sum(n))) %>% 
  ggplot(aes(x = reorder(education, education.num), y = pct, fill = income)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(name = "") +
    scale_y_continuous(name = "Percentage", labels = percent) +
    scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") +
    geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
    ggtitle("Income > 50K by Education") +
    theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
    coord_flip()
grid.arrange(education_graph_distr, education_graph_pct, nrow = 1)

#################################
##### EDA - MARITAL STATUS ######
#################################

marital_status_graph_distr <- training_set %>% 
  group_by(marital.status, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(marital.status, order), y = n)) +
  scale_x_discrete(name = "Marital Status") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of Marital Status") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

marital_status_graph_pct <- training_set %>% 
  group_by(marital.status, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(marital.status, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income > 50K by Marital Status") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()
grid.arrange(marital_status_graph_distr, marital_status_graph_pct, nrow = 1)


#################################
####### EDA - OCCUPATION ########
#################################

occupation_graph_distr <- training_set %>% 
  group_by(occupation, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(occupation, order), y = n)) +
  scale_x_discrete(name = "Occupation") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

occupation_graph_pct <- training_set %>% 
  group_by(occupation, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(occupation, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()
grid.arrange(occupation_graph_distr, occupation_graph_pct, nrow = 1)

#################################
###### EDA - RELATIONSHIP #######
#################################

relationship_graph_distr <- training_set %>% 
  group_by(relationship, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(relationship, order), y = n)) +
  scale_x_discrete(name = "Relationship") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

relationship_graph_pct <- training_set %>% 
  group_by(relationship, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(relationship, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()
grid.arrange(relationship_graph_distr, relationship_graph_pct, nrow = 1)

##################################
########### EDA - RACE ###########
##################################

race_graph_distr <- training_set %>% 
  group_by(race, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(race, order), y = n)) +
  scale_x_discrete(name = "Race") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

race_graph_pct <- training_set %>% 
  group_by(race, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(race, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
grid.arrange(race_graph_distr, race_graph_pct, nrow = 1)

#################################
########### EDA - SEX ###########
#################################

sex_graph_distr <- training_set %>% 
  group_by(sex, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(sex, order), y = n)) +
  scale_x_discrete(name = "Sex") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

sex_graph_pct <- training_set %>% 
  group_by(sex, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(sex, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
grid.arrange(sex_graph_distr, sex_graph_pct, nrow = 1)

#################################
#### EDA - CAPITAL GAIN/LOSS ####
#################################

capitalgain_graph_distr <- training_set %>% 
  ggplot(aes(x=capital.gain)) +
  geom_histogram(binwidth = 10000) +
  scale_x_continuous(name = "Capital Gain") +
  scale_y_continuous(name = "Count") 

capitalgain_graph_pct <- training_set %>% 
  group_by(capital.gain, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  complete(income, fill = list(n = 0, pct = 0)) %>% 
  group_by(capital.gain) %>% 
  mutate(max = max(as.numeric(income))) %>% 
  filter(as.numeric(income)==max) %>% 
  ggplot(aes(x = capital.gain, y = pct)) +
  stat_smooth(geom = "area", method = "loess", span = 1/3, alpha = 1/2, fill = "#9ecae1") +
  scale_y_continuous(name = "Percentage", limits = c(0,1), labels = percent) +
  scale_x_continuous(name = "Capital Gain")

capitalloss_graph_distr <- training_set %>% 
  ggplot(aes(x=capital.loss)) +
  geom_histogram(binwidth = 500) +
  scale_x_continuous(name = "Capital Loss") +
  scale_y_continuous(name = "Count") 

capitalloss_graph_pct <- training_set %>% 
  group_by(capital.loss, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  complete(income, fill = list(n = 0, pct = 0)) %>% 
  group_by(capital.loss) %>% 
  mutate(max = max(as.numeric(income))) %>% 
  filter(as.numeric(income)==max) %>% 
  ggplot(aes(x = capital.loss, y = pct)) +
  stat_smooth(geom = "area", method = "loess", span = 1/3, alpha = 1/2, fill = "#9ecae1") +
  scale_y_continuous(name = "Percentage", limits = c(0,1), labels = percent) +
  scale_x_continuous(name = "Capital Loss")


grid.arrange(capitalgain_graph_distr, capitalgain_graph_pct, capitalloss_graph_distr, capitalloss_graph_pct,nrow = 2, ncol=2)


################################
##### EDA - HOURS PER WEEK #####
################################

hours_per_week_graph_distr <- training_set %>% 
  ggplot(aes(x=hours.per.week)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(name = "Hours per Week", breaks = seq(0,90,10), labels = seq(0,90,10)) +
  scale_y_continuous(name = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

hours_per_week_graph_pct <- training_set %>% 
  group_by(hours.per.week, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  complete(income, fill = list(n = 0, pct = 0)) %>% 
  group_by(hours.per.week) %>% 
  mutate(max = max(as.numeric(income))) %>% 
  filter(as.numeric(income)==max) %>% 
  ggplot(aes(x = hours.per.week, y = pct)) +
  stat_smooth(geom = "area", method = "loess", span = 1/3, alpha = 1/2, fill = "#9ecae1") +
  scale_y_continuous(name = "Percentage", limits = c(0,0.45), labels = percent) +
  scale_x_continuous(name = "Hours per Week", breaks = seq(0,90,10), labels = seq(0,90,10))
grid.arrange(hours_per_week_graph_distr, hours_per_week_graph_pct, nrow = 1)

##################################
###### EDA - NATIVE COUNTRY ######
##################################

native_country_graph_distr <- training_set %>% 
  group_by(native.country, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(native.country, order), y = n)) +
  scale_x_discrete(name = "Native Country") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

native_country_graph_pct <- training_set %>% 
  group_by(native.country, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(native.country, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  coord_flip()
grid.arrange(native_country_graph_distr, native_country_graph_pct, nrow = 1)

###################################
# EXPLORATORY DATA ANALYSIS (END) #
###################################

# REMOVE VARIABLES 'EDUCATION' AND 'FNLWGT'
training_set <- training_set %>% 
  select(-c(education, fnlwgt))

validation_set <- validation_set %>% 
  select(-c(education, fnlwgt))

# ADD NEW VARIABLES 'MARRIED', 'OTHER.INCOME', 'WORKCLASS2', 'HOURS.PER.WEEK2'
training_set <- training_set %>% 
  mutate(married = as.factor(ifelse(marital.status %in% c("Married-civ-spouse", "Married-AF-spouse"), 1, 0)),
         other.income = as.factor(ifelse(capital.gain > 0 | capital.loss > 0, 1, 0)),
         hours.per.week.2 =as.factor(ifelse(hours.per.week > 40, 1, 0)),
         workclass.2 = as.factor(case_when(workclass %in% c("Private") ~ "Private",
                                workclass %in% c("Federal-gov", "Local-gov", "State-gov") ~ "Government",
                                workclass %in% c("Self-emp-inc", "Self-emp-not-inc") ~ "Self-Employed",
                                workclass %in% c("Never-worked", "Without-pay", "N/A") ~ "Other"))) %>% 
  mutate_if(is.factor, fct_explicit_na)

validation_set <- validation_set %>% 
  mutate(married = as.factor(ifelse(marital.status %in% c("Married-civ-spouse", "Married-AF-spouse"), 1, 0)),
         other.income = as.factor(ifelse(capital.gain > 0 | capital.loss > 0, 1, 0)),
         hours.per.week.2 =as.factor(ifelse(hours.per.week > 40, 1, 0)),
         workclass.2 = as.factor(case_when(workclass %in% c("Private") ~ "Private",
                                           workclass %in% c("Federal-gov", "Local-gov", "State-gov") ~ "Government",
                                           workclass %in% c("Self-emp-inc", "Self-emp-not-inc") ~ "Self-Employed",
                                           workclass %in% c("Never-worked", "Without-pay", "N/A") ~ "Other"))) %>% 
  mutate_if(is.factor, fct_explicit_na)

#################################
####### EDA - WORKCLASS.2 #######
#################################

workclass.2_graph_distr <- training_set %>% 
  group_by(workclass.2, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(workclass.2, order), y = n)) +
  scale_x_discrete(name = "Workclass") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of Workclass") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

workclass.2_graph_pct <- training_set %>% 
  group_by(workclass.2, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(workclass.2, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Workclass") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income > 50K by Workclass") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()
grid.arrange(workclass.2_graph_distr, workclass.2_graph_pct, nrow = 1)

#################################
######## EDA - MARRIED.2 ########
#################################

married_graph_distr <- training_set %>% 
  group_by(married, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(married, order), y = n)) +
  scale_x_discrete(name = "married") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of married") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

married_graph_pct <- training_set %>% 
  group_by(married, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(married, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "married") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income > 50K by married") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()
grid.arrange(married_graph_distr, married_graph_pct, nrow = 1)

#################################
##### EDA - HOURS.PER.WEEK.2 ####
#################################

hours.per.week.2_graph_distr <- training_set %>% 
  group_by(hours.per.week.2, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(hours.per.week.2, order), y = n)) +
  scale_x_discrete(name = "hours.per.week.2") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of hours.per.week.2") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

hours.per.week.2_graph_pct <- training_set %>% 
  group_by(hours.per.week.2, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(hours.per.week.2, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "hours.per.week.2") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income > 50K by hours.per.week.2") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()
grid.arrange(hours.per.week.2_graph_distr, hours.per.week.2_graph_pct, nrow = 1)


#################################
####### EDA - OTHER.INCOME ######
#################################

other.income_graph_distr <- training_set %>% 
  group_by(other.income, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(other.income, order), y = n)) +
  scale_x_discrete(name = "other.income") +
  scale_y_continuous(name = "Count") +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of other.income") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

other.income_graph_pct <- training_set %>% 
  group_by(other.income, income) %>% 
  summarize(n = n()) %>% 
  mutate(pct = ifelse(n/sum(n)==0,NA,n/sum(n)), order = ifelse(income==1, n/sum(n), 0)) %>% 
  ggplot(aes(x = reorder(other.income, order), y = pct, fill = income)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "other.income") +
  scale_y_continuous(name= "Percentage", labels = percent) +
  scale_fill_brewer(name = "Income", labels = c("<=50K", ">50K"), palette = "Blues") + 
  geom_text(aes(label = paste0(round(pct*100,0),"%")), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  ggtitle("Income > 50K by other.income") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y=element_blank()) +
  coord_flip()
grid.arrange(other.income_graph_distr, other.income_graph_pct, nrow = 1)



#################################
########## MODELLING ############
#################################

#################################
############# K-NN ##############
#################################

# CREATE TRAINING AND VALIDATION SET ONLY THE NUMERIC VARIABLES AND THE RESPONSE VARAIBLE
training_set_knn <- training_set[,c("age", "education.num", "capital.gain", "capital.loss", "hours.per.week", "income")]
validation_set_knn <- validation_set[,c("age", "education.num", "capital.gain", "capital.loss", "hours.per.week", "income")]

# TRAIN MODEL:
# - METHOD: K-NN
# - TRAIN CONTROL: 10X CROSS-VALIDATION
# - PREPROCESSING: NORMALIZE NUMERICAL VARIABLES
# - NB. OF NEIGHBORS: FROM 11 TO 35 (ODD ONLY)

set.seed(1, sample.kind="Rounding")
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(income ~., data = training_set_knn, method="knn", trControl = trainControl, preProcess = c("center", "scale"), tuneGrid = expand.grid(k = seq(11,35,2)))

# PLOT RESULTS TRANING
plot(knn_fit) # OPTINAL NUMBER OF NEIGHBORS: 23

# CREATE PREDICTIONS AND CONFUSION MATRIX
predict_knn <- predict(knn_fit, newdata = validation_set_knn)
confMatrix_knn <- confusionMatrix(predict_knn, validation_set_knn$income)
confMatrix_knn

#################################
######## RANDOM FOREST ##########
#################################

# CREATE TRAINING AND VALIDATION SET WITH ONLY THE MOST IMPORTANTE VARIABLES BASED ON THE EXPLORATORY DATA ANALYSIS 
training_set_rf <- training_set[,c("age", "education.num", "marital.status", "occupation", "relationship", "sex", "capital.gain", "income")]
validation_set_rf <- validation_set[,c("age", "education.num", "marital.status", "occupation", "relationship", "sex", "capital.gain", "income")]

# TRAIN MODEL:
# - METHOD: RANDOM FOREST

set.seed(1, sample.kind="Rounding")
rf_fit <- randomForest(income~., data = training_set_rf)
rf_fit

# PLOT VARIABLE IMPORTANCE
varImpPlot(rf_fit, main="Variable Importance")

# CREATE PREDICTIONS AND CONFUSION MATRIX
predict_rf <- predict(rf_fit, newdata = validation_set_rf)
confMatrix_rf <- confusionMatrix(predict_rf, validation_set_rf$income)
confMatrix_rf
