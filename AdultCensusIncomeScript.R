# Install and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
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
  select(workclass, income) %>% 
  mutate(workclass = fct_reorder(workclass, as.numeric(income),mean)) %>% 
  ggplot(aes(x = workclass, fill = income)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(name = "Income", labels = c("<=50K", ">50K")) +
  coord_flip()
