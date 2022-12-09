# READ: plots should be saved in "plots/", with descriptive names
#       and optionally, a higher resolution, to be used in report
#       might also help to "beautify" the plots into more eye-pleasing
#       graphs, without compromising anything of importance

library(statsr)
library(dplyr)
library(ggplot2)
library(tidyverse)
# Did I seriously install this for the sole purpose of displaying x-axis on
# the top facet for Female ages? yes
# Also don't forget to install.packages() it :)
library(lemon)

source("generic-functions.R")

# Loading the csv with passenger data into "titanic" data frame
  # Main dataset
titanic <- read.csv("dataset/train.csv")

# Vectors to be passed into matrix summarising function
  # Column names vector (Add/Remove, making sure they exit in passed sample)
matrix_columns <- c("Age", "Fare")

  # Functions to be executed with the passed sample (name = ~ f(., na.rm = T))
matrix_functions <- list(min = ~ min(., na.rm = TRUE),
                          max = ~ max(., na.rm = TRUE),
                          q1 = ~ quantile(., 0.25, na.rm = TRUE),
                          q3 = ~ quantile(., 0.75, na.rm = TRUE),
                          mean = ~ mean(., na.rm = TRUE),
                          sigma = ~ sd(., na.rm = TRUE),
                          iqr = ~ IQR(., na.rm = TRUE))

  # Row names to represent the functions' values for each column
matrix_rows <- c("Minimum Value",
                 "Maximum Value",
                 "First Quartile",
                 "Third Quartile",
                 "Mean Population",
                 "Sigma Population",
                 "Inter Quantile Range")

# Sourcing the matrix function
source("based-matrix.R")

# Testing on NA-filled population
titanic %>% matrix_summary(summary_columns = matrix_columns,
                           summary_functions = matrix_functions,
                           summary_matrix_row = matrix_rows)

# Cleaned dataset (no NANs, Will be working with this.) 4th Requirement done
titanic_clean <- na.omit(titanic)

# Testing on cleaned population
titanic_clean %>% matrix_summary(summary_columns = matrix_columns,
                                 summary_functions = matrix_functions,
                                 summary_matrix_row = matrix_rows)

source("age-functions.R")

# Question 8, to take a random pop sample of age and point estimate the mean
# and standard deviation
pt_estimate_sample <- titanic_clean %>% sample_n(size = 50, replace = TRUE)

summarise(pt_estimate_sample, x_bar = mean(Age), s = sd(Age))

# Questions 9 through 15 (Q12 is theoretical, please write in pdf)
  # Q9: 50 samples of size 50
    # What is required, making a vector of sample_means50
sample_means50 <- titanic_clean %>% sample_mean(sample_size = 50,
                                                sample_reps = 50,
                                                col_name = Age)

sample_mean_plot(passed_sampled_df = sample_means50)

    # What isn't required but I'm just being fancy -Supermjork
titanic_clean %>% sample_plot_mean(passed_size = 50,
                                   passed_reps = 50,
                                   passed_col_name = Age)

  # Q10: 100 samples of size 50

  # Q11: 1000 samples of size 50
