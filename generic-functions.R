# Function to calculate the means from a passed column
sample_mean <- function(sample_passed, sample_size, sample_reps, col_name) {
  sample_passed %>% rep_sample_n(size = sample_size,
                                 reps = sample_reps,
                                 replace = TRUE) %>%
    summarise(x_bar = mean({{col_name}}))
}

# Another fancy function to plot the samples (Has to be used with above fn)
sample_mean_plot <- function(passed_sampled_df) {
  # E(x_bar) = mu
  mean_of_mean <- mean(passed_sampled_df$x_bar)
  # Plotting x_bar
  passed_sampled_df %>% ggplot(aes(x = x_bar)) +
                        labs(title = deparse(substitute(passed_sampled_df))) +
                        geom_histogram(binwidth = 0.25,
                                       aes(fill = after_stat(count))) +
                        scale_fill_continuous(high = "#003b94",
                                              low = "#6ac2eb") +
                        geom_vline(aes(xintercept = mean_of_mean),
                                   colour = "red",
                                   linetype = "dashed",
                                   linewidth = 1) +
                        geom_text(x = mean_of_mean,
                                  y = Inf,
                                  vjust = 1,
                                  hjust = "inward",
                                  aes(label = paste("Estimated mean = ",
                                                    mean_of_mean)))
}

# The function above But WAYYY BETTER
# FOR YOU MY FRIEND THE BEST!
sample_plot_mean <- function(sample_df,
                             passed_size,
                             passed_reps,
                             passed_col_name) {
  sample_in_fn <- sample_df %>% sample_mean(sample_size = passed_size,
                                            sample_reps = passed_reps,
                                            col_name = {{passed_col_name}})
  sample_in_fn %>% ggplot(aes(x = x_bar)) +
                   labs(title = deparse(substitute(passed_sampled_df))) +
                   geom_histogram(binwidth = 0.25,
                                  aes(fill = after_stat(count))) +
                   scale_fill_continuous(high = "#003b94",
                                         low = "#6ac2eb") +
                   geom_vline(aes(xintercept = mean(x_bar)),
                              colour = "red",
                              linetype = "dashed",
                              linewidth = 1) +
                   geom_text(x = mean(sample_in_fn$x_bar),
                             y = Inf,
                             vjust = 1,
                             hjust = "inward",
                             aes(label = paste("Estimated mean = ",
                                               mean(x_bar))))
}

# Function to save plots, will take the plot variable as first argument (pipe)
# extension is hardcoded to .pdf
# will be defaulting the name to the variable's name because yes
# takes width and height, you can specify the unit or we could hardcode it
save_plot <- function(plot_to_save, width, height, foldername) {
  # Creating the file first for ggsave() to be able to open
  file.create(paste0(foldername, "/", plot_to_save$labels$title, ".pdf"))
  # The actual save with passed parameters
  ggsave(filename = paste0(plot_to_save$labels$title, ".pdf"),
         plot = plot_to_save,
         path = foldername,
         width = width, height = height, units = "px")
}

# Function to divide the population into ranges
populus_range <- function(pop_df, pass_step, grouping_column, ranging_column) {
  # Getting minimum and maximum values of column that will be sliced
  col_min <- round(min(pop_df[[ranging_column]]))
  col_max <- max(pop_df[[ranging_column]])
  pop_df$grouped_col <- cut(pop_df[[ranging_column]],
                                breaks = seq(from = col_min,
                                             to = col_max,
                                             by = pass_step),
                                right = TRUE)

  # Colour gradient for Population age ranges (M/F) H:681ab0, L:d062f5
  population_age <- pop_df %>% ggplot(aes(x = grouped_col,
                                          fill = after_stat(count))) +
                               labs(title = paste0({{ranging_column}},
                                                   " Range Plot"),
                                    x = paste0({{ranging_column}},
                                               " Ranges")) +
                               scale_fill_continuous(high = "#681ab0",
                                                     low = "#d062f5") +
                               geom_bar() +
                               geom_text(stat = "count",
                                         aes(label = after_stat(count),
                                             vjust = "inwards"))
  # Colour gradient for Male age ranges H: 2b61e0, L:57beeb
  male_pop_range <- pop_df %>% group_by(Sex) %>%
                    filter(any(Sex == "male")) %>%
                    ggplot(aes(x = grouped_col,
                               fill = after_stat(count))) +
                    labs(title = paste0("Male ", {{ranging_column}},
                                        " Range Plot"),
                         x = paste0("Male ", {{ranging_column}},
                                    " Ranges")) +
                    scale_fill_continuous(high = "#2b61e0",
                                          low = "#57beeb") +
                    geom_bar() +
                    geom_text(stat = "count",
                              aes(label = after_stat(count)),
                              vjust = "inward")
  # Colour gradient for Female age ranges H:960661, L:de47a6
  female_pop_range <- pop_df %>% group_by(Sex) %>%
                      filter(any(Sex == "female")) %>%
                      ggplot(aes(x = grouped_col,
                                 fill = after_stat(count))) +
                      labs(title = paste0("Female ",
                                          {{ranging_column}},
                                          " Range Plot"),
                           x = paste0("Female ",
                                      {{ranging_column}},
                                      " Ranges")) +
                      scale_fill_continuous(high = "#960661",
                                            low = "#de47a6") +
                      geom_bar() +
                      geom_text(stat = "count",
                                aes(label = after_stat(count)),
                                vjust = "inward")
  combined_plots <- list(population_age, male_pop_range, female_pop_range)
  ggarrange(plotlist = combined_plots,
            ncol = 1,
            nrow = 3)
}

# Function to take 2 dataframes, calculate the difference between their means
# of a specified column
mean_difference <- function(passed_df0, passed_df1, col_name) {
  # creating dataframes with sample mean of given column
  df0_means <- passed_df0 %>% summarise(x_bar = mean({{col_name}}))
  df1_means <- passed_df1 %>% summarise(x_bar = mean({{col_name}}))

  # Merging the dataframes into one chunk
  result_df <- merge(df0_means, df1_means, by = "replicate")

  # Getting the difference between the columns
  result_df$x_bar_diff <- result_df$x_bar.x - result_df$x_bar.y
  avg_diff <- mean(result_df$x_bar_diff)

  if (avg_diff > 0) {
    print(paste0("The average difference between ages: ", avg_diff))
    print(paste0("The males were relatively older."))
  } else {
    print(paste0("The average difference between ages: ", avg_diff))
    print(paste0("The females were relatively older."))
  }
  # Plotting the difference
  result_df %>% ggplot(aes(x = x_bar_diff)) +
                labs(x = " Mean Difference") +
                geom_histogram(binwidth = 0.25, 
                               aes(fill = after_stat(count))) +
                geom_vline(aes(xintercept = avg_diff),
                           colour = "red",
                           linetype = "dashed",
                           linewidth = 1) +
                scale_fill_continuous(high = "#003b94",
                                      low = "#6ac2eb") +
                geom_text(x = avg_diff,
                          y = Inf,
                          vjust = 1,
                          aes(label = paste0("Average difference: ", avg_diff)))
}

survival_difference <- function(passed_df0, passed_df1) {
  # Counting the survivors/non-survivors in each sample in df0
  count_survival_all0 <- passed_df0 %>% count(Survived == 1)
  # Getting count of survivors in each sample
  count_survival_true0 <- count_survival_all0[seq(2,
                                                  nrow(count_survival_all0),
                                                  2), ]
  # Getting count of non-survivors in each sample
  count_survival_false0 <- count_survival_all0[seq(1,
                                                   nrow(count_survival_all0),
                                                   2), ]
  # Counting survivors/non-survivors in each sample in df1
  count_survival_all1 <- passed_df1 %>% count(Survived == 1)
  # Getting count of survivors in each sample
  count_survival_true1 <- count_survival_all1[seq(2,
                                                  nrow(count_survival_all1),
                                                  2), ]
  # Getting count of non-survivors in each sample
  count_survival_false1 <- count_survival_all1[seq(1,
                                                   nrow(count_survival_all1),
                                                   2), ]
  # Merging the two count-dfs of survivors
  has_survived <- merge(count_survival_true0,
                        count_survival_true1,
                        by = "replicate")
  # Calculating the difference between survivors count
  has_survived$rescue_diff <- has_survived$n.x - has_survived$n.y
  # Getting an average amount of survivors
  rescue_avg <- mean(has_survived$rescue_diff)
  if (rescue_avg > 0) {
    print(paste0("The difference in survivors on average was: ",
                 ceiling(abs(rescue_avg))))
    print(paste0("With males being rescued more,"))
  } else {
    print(paste0("The difference in survivors on average was: ",
                 ceiling(abs(rescue_avg))))
    print(paste0("With females being rescued more."))
  }
  # Survivability percentages from sample, i have no idea so i'll push
  # really simple, trust
  survival_df0_percentage <- sum(count_survival_true0$n) / sum(count_survival_all0$n)
  survival_df1_percentage <- sum(count_survival_true1$n) / sum(count_survival_all1$n)
  print(paste0("Survival Rate for males: ", survival_df0_percentage, "%"))
  print(paste0("Survival Rate for females: ", survival_df1_percentage, "%"))
  # Plotting the difference
  has_survived %>% ggplot(aes(x = rescue_diff)) +
                   labs(x = "Survival Difference") +
                   geom_bar(aes(fill = after_stat(count))) +
                   geom_vline(aes(xintercept = rescue_avg),
                              colour = "red",
                              linetype = "dashed",
                              linewidth = 1) +
                   scale_fill_continuous(high = "#003b94",
                                         low = "#6ac2eb") +
                   geom_text(x = rescue_avg,
                             y = Inf,
                             vjust = 1,
                             aes(label = paste0("Average Rescues: ",
                                                rescue_avg)))
}

constant_coeff <- function(sample, constant, coefficient, col_name) {
  sample$multiply_coeff <- sample[[col_name]] * coefficient
  sample$result_x <- sample$multiply_coeff + constant
  expectation_x <- mean(sample$result_x)
  variance_x <- var(sample$result_x)
  print(paste0("E(", {{col_name}}, "): ", expectation_x))
  print(paste0("V(", {{col_name}}, "): ", variance_x))
}

random_sample <- function(population, sample_size, col_name) {
  sample <- sample_n(population, size = sample_size)
  sample <- sample[[col_name]]
  x_bar <- mean(sample)
  s <- sqrt( var(sample) / n)
  return(c(x_bar, s))
}
