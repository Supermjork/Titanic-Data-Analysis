# READ: plots should be saved in "plots/", with descriptive names
#       and optionally, a higher resolution, to be used in report
#       might also help to "beautify" the plots into more eye-pleasing
#       graphs, without compromising anything of importance

source("init.R")

# Couting passengers which embark from certain areas
# S: Southampton, Q: Queenstown, C: Cherbourg
embarked_s <- colSums(titanic == "S")
embarked_s

embarked_q <- colSums(titanic == "Q")
embarked_q

embarked_c <- colSums(titanic == "C")
embarked_c

embarked_s_percent <- round(embarked_s / nrow(titanic) * 100)
embarked_s_percent

embarked_q_percent <- round(embarked_q / nrow(titanic) * 100)
embarked_q_percent

embarked_c_percent <- round(embarked_c / nrow(titanic) * 100)
embarked_c_percent

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

# Counting how many NAs are in the dataset's columns
na_in_df <- colSums(is.na(titanic))

na_in_df

# Plotting the range of ages
age_range_by_gender <- populus_range(titanic_clean, 10, "Sex", "Age")
age_range_by_gender

# Fare related info/plots
juan_plot
dos_plot
tres_plot

combined_fare <- ggarrange(juan_plot,
                           dos_plot,
                           tres_plot,
                           nrow = 3,
                           ncol = 1)
combined_fare

fare_total <- fare_populus(titanic_clean)
fare_total

# Plotting the population's ages (Must pick environment variables from
# age-functions.R and insert into list (i.e. layers on top of basic graph))
age_fancyplot_gender + list(gender_mean_age,
                            age_plot_sex_grid,
                            age_plot_gender_text)

age_fancyplot_location + list(embarked_mean_age,
                              age_plot_embarked_grid,
                              age_plot_embarked_text)

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
      # Basically takes in the population and does what the other two functions
      # do, get mean and plot
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

  # Combined plot of any amount of plots above
means_50all_combined <- list(means_50_plot, means_100_plot, means_1000_plot)

combined_plot_size50 <- ggarrange(plotlist = means_50all_combined,
                                  ncol = 1,
                                  nrow = length(means_50all_combined))

combined_plot_size50

# Variance stuff
sample_u1500 <- sample_variance(sample_passed = titanic_clean,
                                sample_size = 2,
                                sample_reps = 1500,
                                col_name = Age)
var_plot_s2 <- sample_var_plot(sample_u1500)

# This my friend looks like a chi tho (it do, chi of 1)
var_plot_s2

sample_u1500 <- sample_variance(sample_passed = titanic_clean,
                                sample_size = 50,
                                sample_reps = 1500,
                                col_name = Age)
var_plot_s50 <- sample_var_plot(sample_u1500)
# Normal Distrib lookin (Actually Chi?) (yeeee chi of 49 lmao)
var_plot_s50

# Estimators time

# age_sample50 <- sample_n(titanic_clean, size = 50) # This code sucks
# mme_age_sample50 <- mean(age_sample50$Age)
# mme_age_sample50_bias <- mme_age_sample50 - mean(titanic$Age)

  # MME
mme_age_sample50 <- mme_estimator(population = titanic_clean,
                                            sample_size = 50,
                                            col_name = "Age")
mme_age_sample50

  #MLE
mle_age_sample50 <- mle_estimator(population = titanic_clean,
                                  sample_size = 50,
                                  col_name = "Age")
mle_age_sample50$par
mean_square_error(mle_age_sample50, mme_age_sample50, titanic_clean, "Age")


# Q21 time
age_male_20221445850 <- titanic_clean %>%
                        group_by(Sex) %>%
                        filter(any(Sex == "male")) %>%
                        rep_sample_n(size = 50, reps = 15000, replace = TRUE)

age_female_20221372981 <- titanic_clean %>%
                          group_by(Sex) %>%
                          filter(any(Sex == "female")) %>%
                          rep_sample_n(size = 50, reps = 15000, replace = TRUE)

samplediff_means15000 <- mean_difference(age_male_20221445850,
                                         age_female_20221372981,
                                         Age)

samplediff_means15000

# Q22
survived_male <- titanic_clean %>%
                 group_by(Sex) %>%
                 filter(any(Sex == "male")) %>%
                 rep_sample_n(size = 50, reps = 15000, replace = TRUE)

survived_female <- titanic_clean %>%
                   group_by(Sex) %>%
                   filter(any(Sex == "female")) %>%
                   rep_sample_n(size = 50, reps = 15000, replace = TRUE)

samplediff_survived15000 <- survival_difference(survived_male, survived_female)

samplediff_survived15000

test_expectation <- titanic_clean %>% sample_n(size = 200)

mean(test_expectation$Age)
var(test_expectation$Age)

constant_coeff(test_expectation, 5, 1, "Age")
constant_coeff(test_expectation, 0, 5, "Age")

# Take a sample of ages size 10 and 50
age_sample10 <- random_sample(titanic_clean, 10, "Age")
age_sample50 <- random_sample(titanic_clean, 50, "Age")

# Confidence Intervals
conf_interval2(age_sample10, 10)
conf_interval2(age_sample50, 10)