# Your histogram be boring (Also doubt the frequency numbers)
hist(titanic$Age, xlab = "Ages", main = paste("Histogram of Age Frequency"))

# Shows the population's mean for Age
age_mean_gender <- titanic_clean %>%
                   group_by(Sex) %>%
                   summarise_at(vars(Age),
                                list(mean = mean))

age_mean_embark <- titanic_clean %>%
                   group_by(Embarked) %>%
                   summarise_at(vars(Age),
                                list(mean = mean))

# Setting position to dodge to have the lines beside
# each other instead of overlapping (tinker around binwidth)
# Used it over "stack" to show true count of people at age
# Stack just sums both up to a accumulated count
# Setting the bars' position relative to each other
hist_pos <- "dodge"

# Setting the legend's position (Only for when working with both genders)
# i.e. the fancy plot
leg_pos <- "right"

# Plot representing the amount of passengers and shows their gender
# Shows absolute mean AND Trying to show mean for either genders

  # The basic (allegedly) plot with ages shown by gender and legend
age_fancyplot_gender <- titanic_clean %>%
                        ggplot(aes(x = Age,
                                   color = Sex,
                                   fill = Sex)) +
                        geom_histogram(binwidth = 1,
                                       alpha = 0.25,
                                       position = hist_pos) +
                        labs(title = "Age mean by genders",
                             y = "count") +
                        theme(legend.position = leg_pos)

age_fancyplot_location <- titanic_clean %>%
                          ggplot(aes(x = Age,
                                     colour = Embarked,
                                     fill = Embarked)) +
                          geom_histogram(binwidth = 1,
                                         alpha = 0.25,
                                         position = hist_pos) +
                          labs(title = "Age mean by Embarked Location",
                               y = "count") +
                          theme(legend.position = leg_pos)
  # Shows the mean of ages by gender (Males' mean age, Females' mean age)
gender_mean_age <- geom_vline(data = age_mean_gender,
                              aes(xintercept = mean,
                                  colour = Sex),
                              linetype = "dashed",
                              linewidth = 1)

embarked_mean_age <- geom_vline(data = age_mean_embark,
                                aes(xintercept = mean,
                                    colour = Embarked),
                                linetype = "dashed",
                                linewidth = 1)

  # Shows the absolute average of ages in the population
  # (Mean Age of both genders)
age_absmean <- geom_vline(aes(xintercept = mean(Age)),
                          colour = "red",
                          linetype = "dashed",
                          linewidth = 1)

  # Facets the graph into 2 grids for each gender
age_plot_sex_grid <- facet_rep_grid(Sex ~ ., scales = "free",
                                    repeat.tick.labels = TRUE)

age_plot_embarked_grid <- facet_rep_grid(Embarked ~ ., scales = "free",
                                         repeat.tick.labels = TRUE)

  # Texts to follow mean lines
age_plot_gender_text <- geom_text(data = age_mean_gender,
                                  aes(x = mean,
                                      y = Inf,
                                      vjust = 1,
                                      label = paste0("Estimated Mean for ", Sex,
                                                     " Age: ",
                                                     signif(mean, 5))),
                                      show.legend = FALSE,
                                      colour = "black")

age_plot_embarked_text <- geom_text(data = age_mean_embark,
                                    aes(x = mean,
                                        y = Inf,
                                        vjust = 1,
                                        label = paste0("Estimated Mean for ",
                                                       Embarked, " passengers'",
                                                       " age: ",
                                                       signif(mean, 5))),
                                    show.legend = FALSE,
                                    colour = "black")

  # Write which layers to add onto the plot
age_fancyplot_gender + list(gender_mean_age,
                            age_plot_sex_grid,
                            age_plot_gender_text)

age_fancyplot_location + list(embarked_mean_age,
                              age_plot_embarked_grid,
                              age_plot_embarked_text)

# Boring basic histogram
# Straight up plot for age distribution
# Also shows absolute mean
# Will probs have to pipe into rep_sample_n cuz it's required instead of pop
age_plot <- titanic_clean %>% ggplot(aes(x = Age)) +
                              geom_histogram(binwidth = 1) +
                              geom_vline(aes(xintercept = mean(Age)),
                                         colour = "red",
                                         linetype = "dashed",
                                         linewidth = 1)
age_plot
