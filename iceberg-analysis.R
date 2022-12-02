# READ: plots should be saved in "plots/", with descriptive names
#       and optionally, a higher resolution, to be used in report
#       might also help to "beautify" the plots into more eye-pleasing
#       graphs, without compromising anything of importance

library(statsr)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Loading the csv with passenger data into "titanic" data frame
  # Main dataset
titanic <- read.csv("dataset/train.csv")

# Answer to Q1, using summarize function
# Prior to answering, it is more optimal to remove the NaN data fields
# Unsure if one should do this prior to summarise function
  # Cleaned dataset (no NANs)
titanic_clean = na.omit(titanic)

# Trying to summarise multiple columns
  # Putting summarised data into variable
titanic_summarise <- titanic_clean %>% summarise_at(c("Age", "Fare"),
                                                    list(min = min,
                                                         max = max,
                                                         #q1 = quantile(0.25),  Error here, needs passed vector (Column in our case duh)
                                                         mean = mean,
                                                         sigma = sd),
                                                    na.rm = TRUE)
titanic_summarise
  
  # Printing summarised data in a neat manner instead of 1 continuous row
matrix(titanic_summarise, nrow = 4, ncol = 2,
       dimnames = list(c("min", "max", "mean", "sigma"), c("Age", "Fare")),
       byrow = TRUE)

# Summarising each column individually
  # First summary regards Age
titanic_clean %>% summarise(age_min = min(Age),
                            age_max = max(Age),
                            age_q1 = quantile(Age, 0.25),
                            age_q3 = quantile(Age, 0.75),
                            age_med = median(Age),
                            age_mu = mean(Age),
                            age_sigma = sd(Age),
                            age_iqr = IQR(Age))

  # Second summary regards Fare
titanic_clean %>% summarize(fare_min = min(Fare),
                            fare_max = max(Fare))
