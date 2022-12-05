# READ: plots should be saved in "plots/", with descriptive names
#       and optionally, a higher resolution, to be used in report
#       might also help to "beautify" the plots into more eye-pleasing
#       graphs, without compromising anything of importance

library(statsr)
library(dplyr)
library(ggplot2)
# Yes we can ignore this library
# I removed the library
# Loading the csv with passenger data into "titanic" data frame
  # Main dataset
titanic <- read.csv("dataset/train.csv")

# Answer to Q1, using summarize function
  # Trying to summarise multiple columns
    # Putting labels, functions into vectors and list for encapsulation
    # (Would've been better to just use na.omit(titanic) tbh but it's in Q4)
summarised_columns <- c("Age", "Fare")
summary_functions <- list(min = ~ min(., na.rm = TRUE),
                          max = ~ max(., na.rm = TRUE),
                          q1 = ~ quantile(., 0.25, na.rm = TRUE),
                          q3 = ~ quantile(., 0.75, na.rm = TRUE),
                          mean = ~ mean(., na.rm = TRUE),
                          sigma = ~ sd(., na.rm = TRUE),
                          iqr = ~ IQR(., na.rm = TRUE))

summary_matrix_row <- c("Minimum Value",
                        "Maximum Value",
                        "First Quartile",
                        "Third Quartile",
                        "Mean Population",
                        "Sigma Population",
                        "Inter Quantile Range")

    # Putting summarised data into variable
titanic_summarise <- titanic %>% summarise_at(summarised_columns,
  summary_functions)
#titanic_summarise
    # Printing summarised data in a neat manner instead of 1 continuous row
matrix(titanic_summarise, nrow = length(summary_functions),
       ncol = length(summarised_columns),
       dimnames = list(summary_matrix_row, summarised_columns),
       byrow = TRUE)

# Cleaned dataset (no NANs, Will be working with this.) 4th Requirement done
titanic_clean <- na.omit(titanic)

# Your histogram be boring (Also doubt the frequency numbers)
hist(titanic$Age)

# Plot representing the amount of passengers and shows their gender
# Shows absolute mean AND Trying to show mean for either genders

# Setting position to dodge to have the lines beside
# each other instead of overlapping (tinker around binwidth)
# Used it over "stack" to show true count of people at age
# Stack just sums both up to a accumulated count
age_mean_gender <- titanic_clean %>%
  group_by(Sex) %>%
  summarise_at(vars(Age))
  list(mean = mean)

# Setting the bars' position relative to each other
hist_pos <- "dodge"

# Setting the legend's position (Only for when working with both genders)
# i.e. the fancy plot
leg_pos <- "right"

# The basic (allegedly) plot with ages shown by gender and legend
age_fancyplot <- titanic_clean %>% ggplot(aes(x = Age,
                                             color = Sex,
                                             fill = Sex)) +
                                       geom_histogram(binwidth = 1,
                                                      alpha = 0.25,
                                                      position = hist_pos) +
                                       theme(legend.position = leg_pos)

# Shows the mean of ages by gender (Males' mean age, Females' mean age)
gender_mean_age <- geom_vline(data = age_mean_gender,
                                 aes(xintercept = mean,
                                     colour = Sex),
                                 linetype = "dashed",
                                 linewidth = 1)

# Shows the absolute average of ages in the population
#(Mean Age of both genders)
gender_absmean <- geom_vline(aes(xintercept = mean(Age),
                                   colour = Sex),
                               colour = "red",
                               linetype = "dashed",
                               linewidth = 1)

# Facets the graph into 2 grids for each gender
age_plot_grid <- facet_grid(Sex ~ .)

# Write which layers to add onto the plot
age_fancyplot + list(gender_mean_age, age_plot_grid)

# Boring basic histogram
# Straight up plot for age distribution
# Also shows absolute mean
age_plot <- titanic_clean %>% ggplot(aes(x = Age)) +
                                  geom_histogram(binwidth = 1) +
                                  geom_vline(aes(xintercept = mean(Age)),
                                                 colour = "red",
                                                 linetype = "dashed",
                                                 linewidth = 1)

age_plot
