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
validation_index <- createDataPartition(y = clean_data$income, times = 1, p = 0.1, list = FALSE)
train <- clean_data[-validation_index,]
validation <- clean_data[validation_index,]

clean_data %>% 
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
  


clean_data %>% 
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
