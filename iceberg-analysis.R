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
  # Trying to summarise multiple columns
    # Putting labels, functions into vectors and list for encapsulation
summarised_columns <- c("Age", "Fare")
summary_functions <- list(min = min,
                          max = max,
                          q1 =~ quantile(., 0.25),
                          q3 =~ quantile(., 0.75),
                          mean = mean,
                          sigma = sd)
summary_matrix_row <- c("Minimum Value",
                        "Maximum Value", 
                        "First Quartile", 
                        "Third Quartile", 
                        "Mean Population",
                        "Sigma Population")

    # Putting summarised data into variable
titanic_summarise <- titanic %>% summarise_at(summarised_columns,
                                                    summary_functions,
                                                    na.rm = TRUE)
titanic_summarise
  
    # Printing summarised data in a neat manner instead of 1 continuous row
matrix(titanic_summarise, nrow = length(summary_functions),
       ncol = length(summarised_columns),
       dimnames = list(summary_matrix_row, summarised_columns),
       byrow = TRUE)

  # Alt method: Summarising each column individually
    # First summary regards Age
titanic %>% summarise(age_min = min(Age),
                            age_max = max(Age),
                            age_q1 = quantile(Age, 0.25),
                            age_q3 = quantile(Age, 0.75),
                            age_med = median(Age),
                            age_mu = mean(Age),
                            age_sigma = sd(Age),
                            age_iqr = IQR(Age))

  # Second summary regards Fare
titanic %>% summarize(fare_min = min(Fare),
                            fare_max = max(Fare),
                            fare_q1 = quantile(Fare, 0.25),
                            fare_q3 = quantile(Fare, 0.75),
                            fare_med = median(Fare),
                            fare_mu = mean(Fare),
                            fare_sigma = sd(Fare),
                            fare_iqr = IQR(Fare))

# Cleaned dataset (no NANs, Will be working with this)
titanic_clean = na.omit(titanic)
