# READ: plots should be saved in "plots/", with descriptive names
#       and optionally, a higher resolution, to be used in report
#       might also help to "beautify" the plots into more eye-pleasing
#       graphs, without compromising anything of importance

library(statsr)
library(dplyr)
library(ggplot2)
# Yes we can ignore this library
# I removed the library
library(tidyverse)
#I put it back
# Loading the csv with passenger data into "titanic" data frame
  # Main dataset
titanic <- read.csv("dataset/train.csv")

source("based-matrix.R")
# matrix_summary(titanic)
# Cleaned dataset (no NANs, Will be working with this.) 4th Requirement done
titanic_clean <- na.omit(titanic)
#matrix_summar(titanic_clean)

# Question 8, to take a random pop sample of age and point estimate the mean
# and standard deviation
pt_estimate_sample <- titanic_clean %>% sample_n(size = 50, replace = TRUE)

summarise(pt_estimate_sample, x_bar = mean(Age), s = sd(Age))

# Questions 9 through 15 (Q12 is theoretical, please write in pdf)
# Fancy function to summarise the Ages according to samples and reps
source("generic-functions.R")
source("age-functions.R")
# Za chonky function to sample and plot said sample's means, useless

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
