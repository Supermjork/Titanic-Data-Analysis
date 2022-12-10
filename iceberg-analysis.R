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
library(ggpubr)

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

means_50_plot <- sample_mean_plot(passed_sampled_df = sample_means50)

means_50_plot

    # What isn't required but I'm just being fancy -Supermjork
titanic_clean %>% sample_plot_mean(passed_size = 50,
                                   passed_reps = 50,
                                   passed_col_name = Age)

  # Q10: 100 samples of size 50
sample_means100 <- titanic_clean %>% sample_mean(sample_size = 50,
                                                 sample_reps = 100,
                                                 col_name = Age)

means_100_plot <- sample_mean_plot(passed_sampled_df = sample_means100)

means_100_plot

  # Q11: 1000 samples of size 50
sample_means1000 <- titanic_clean %>% sample_mean(sample_size = 50,
                                                  sample_reps = 1000,
                                                  col_name = Age)

means_1000_plot <- sample_mean_plot(passed_sampled_df = sample_means1000)

means_1000_plot

  # Q13: 1500 samples of size 20
sample_means_s20 <- titanic_clean %>% sample_mean(sample_size = 20,
                                                  sample_reps = 1500,
                                                  col_name = Age)

means_s20_plot <- sample_mean_plot(passed_sampled_df = sample_means_s20)

means_s20_plot

  # Q14: 1500 samples of size 100
sample_means_s100 <- titanic_clean %>% sample_mean(sample_size = 100,
                                                   sample_reps = 1500,
                                                   col_name = Age)

means_s100_plot <- sample_mean_plot(passed_sampled_df = sample_means_s100)

means_s100_plot

  # Q15: 1500 samples of size 200
sample_means_s200 <- titanic_clean %>% sample_mean(sample_size = 200,
                                                   sample_reps = 1500,
                                                   col_name = Age)

means_s200_plot <- sample_mean_plot(passed_sampled_df = sample_means_s200)

means_s200_plot

  # Optional: Combining all previous plots based on their size/reps
  #           looks scuffed asf pls ignore, but the idea is there ig
means_50all_combined <- list(means_50_plot, means_100_plot, means_1000_plot)

combined_plot_size50 <- ggarrange(plotlist = means_50all_combined,
                                  labels = c("Size 50, Reps 50",
                                             "Size 50, Reps 100",
                                             "Size 50, Reps 1000"),
                                  ncol = 1, nrow = 3)

combined_plot_size50
sample_u1500 <- sample_variance(sample_passed = titanic_clean,
                                sample_size = 2,
                                sample_reps = 1500,
                                col_name = Age)
var_plot_s2 <- sample_var_plot(sample_u1500)
var_plot_s2
sample_u1500 <- sample_variance(sample_passed = titanic_clean,
                                sample_size = 2,
                                sample_reps = 1500,
                                col_name = Age)
var_plot_s50 <- sample_var_plot(sample_u1500)
var_plot_s50
