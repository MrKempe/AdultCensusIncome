if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

raw_data <- read_csv("https://raw.githubusercontent.com/MrKempe/AdultCensusIncome/master/adult.csv")
table(raw_data$native.country)

raw_data_2 <- na_if(raw_data, "?")
