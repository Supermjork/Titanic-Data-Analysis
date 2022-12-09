# Your histogram be boring (Also doubt the frequency numbers)
hist(titanic$Age, xlab = "Ages", main = paste("Histogram of Age Frequency"))

# Plot representing the amount of passengers and shows their gender
# Shows absolute mean AND Trying to show mean for either genders

# Setting position to dodge to have the lines beside
# each other instead of overlapping (tinker around binwidth)
# Used it over "stack" to show true count of people at age
# Stack just sums both up to a accumulated count
age_mean_gender <- titanic_clean %>%
                   group_by(Sex) %>%
                   summarise_at(vars(Age), list(mean = mean))

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
# (Mean Age of both genders)
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

