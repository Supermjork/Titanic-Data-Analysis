# READ: plots should be saved in "plots/", with descriptive names
#       and optionally, a higher resolution, to be used in report
#       might also help to "beautify" the plots into more eye-pleasing
#       graphs, without compromising anything of importance

library(statsr)
library(dplyr)
library(ggplot2)
# Yes we can ignore this library
#library(tidyverse)

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

# Setting position to doge to have the lines beside
# each other instead of overlapping (tinker around binwidth)
# Used it over "stack" to show true count of people at age
# Stack just sums both up to a accumulated count
age_mean_bySex <- aggregate(titanic_clean$Age, list(titanic_clean$Sex), mean)

# Setting the bars' position relative to each other
age_pop_pos <- "dodge"

# Setting the legend's position (Only for when working with both genders)
# i.e. the fancy plot
age_pop_legend_pos <- "right"

# The basic (allegedly) plot with ages shown by gender and legend
age_pop_fancyplot <- titanic_clean %>% ggplot(aes(x = Age,
                                             color = Sex,
                                             fill = Sex)) +
                                       geom_histogram(binwidth = 1,
                                                      alpha = 0.25,
                                                      position = age_pop_pos) +
                                       theme(legend.position = age_pop_legend_pos)

# Shows the mean of ages by gender (Males' mean age, Females' mean age)
age_show_meanBySex <- geom_vline(data = age_mean_bySex,
                                 aes(xintercept = x,
                                     colour = Group.1),
                                 linetype = "dashed",
                                 linewidth = 1)

# Shows the absolute average of ages in the population (Mean Age of both genders)
age_show_absMean <- geom_vline(aes(xintercept = mean(Age),
                                   colour = Sex),
                               colour = "red",
                               linetype = "dashed",
                               linewidth = 1)

# Facets the graph into 2 grids for each gender
age_facet_plots <- facet_grid(Sex ~ .)

# Write which layers to add onto the plot
age_pop_fancyplot + list(age_show_meanBySex, age_facet_plots)

# Boring basic histogram
# Straight up plot for age distribution
# Also shows absolute mean
age_pop_plot <- titanic_clean %>% ggplot(aes(x = Age)) +
                                  geom_histogram(binwidth = 1) +
                                  geom_vline(aes(xintercept = mean(Age)),
                                                 colour = "red",
                                                 linetype = "dashed",
                                                 linewidth = 1)

age_pop_plot
