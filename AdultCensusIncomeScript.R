# Install and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")

# Set ggplot theme
ds_theme_set()

# Read raw data from Github repository
raw_data <- read.csv("https://raw.githubusercontent.com/MrKempe/AdultCensusIncome/master/adult.csv", stringsAsFactors=TRUE)

# Replace '?' with NA
clean_data <- na_if(raw_data, "?") %>% 
  mutate_if(is.factor, fct_explicit_na) %>% 
  droplevels()
  
# Convert the Income column to a factor 
clean_data <- clean_data %>% 
  mutate(income = as.factor(ifelse(income=="<=50K",0,1))) 

str(clean_data)
head(clean_data)

clean_data %>% 
  group_by(workclass, income) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=workclass, y=n, fill=income)) +
    geom_bar(position="fill", stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(name = "Income", labels = c("<=50K", ">50K")) +
    coord_flip()

clean_data %>% 
  filter(!is.na(workclass)) %>% 
  group_by(workclass, income) %>% 
  summarize(n=n()) %>% 
  mutate(freq = n / sum(n)) %>%
  spread(income,n) %>% 
  mutate(`0`= ifelse(is.na(`0`), 0, `0`), `1`= ifelse(is.na(`1`), 0, `1`)) %>% 
  gather(key=income, value = freq, 2:3) %>% 
  ggplot(aes(x=reorder(workclass, -n), y=freq, fill=income)) +
    geom_bar(position="fill", stat="identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(name = "Income", labels = c("<=50K", ">50K")) +
    coord_flip()

df = df %>% mutate_if(is.factor,
                      fct_explicit_na,
                      na_level = "to_impute")

clean_data2 <- clean_data %>% 
  
str(clean_data2)
levels(clean_data2$workclass)
table(clean_data$workclass)
table(clean_data2$workclass)
