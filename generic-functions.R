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
                   geom_histogram(binwidth = 0.25,
                                  aes(fill = after_stat(count))) +
                   geom_vline(aes(xintercept = mean(x_bar)),
                              colour = "red",
                              linetype = "dashed",
                              linewidth = 1) +
                   geom_text(x = mean(sample_in_fn$x_bar),
                             y = Inf,
                             vjust = 1,
                             aes(label = paste("Estimated mean = ",
                                               mean(x_bar))))
}

# Function to calculate the variance of a sample
sample_variance <- function(sample_passed, sample_size, sample_reps, col_name) {
  sample_passed %>% rep_sample_n(size = sample_size,
                                 reps = sample_reps,
                                 replace = TRUE) %>%
    summarise(s_squared = var({{col_name}}))
}

# Plotting the variance from a passed df of calculated variances
sample_var_plot <- function(passed_sampled_df) {
  # E(s^2) = \sigma^2
  sample_variance_exp <- mean(passed_sampled_df$s_squared)
  
  # Plotting the variance's distribution
  passed_sampled_df %>% ggplot(aes(x = s_squared)) +
                        labs(title = deparse(substitute(passed_sampled_df))) +
                        geom_histogram(binwidth = 0.25,
                                       aes(fill = after_stat(count))) +
                        scale_fill_continuous(high = "#003b94",
                                              low = "#6ac2eb") +
                        geom_vline(aes(xintercept = sample_variance_exp),
                                   colour = "red",
                                   linetype = "dashed",
                                   linewidth = 1) +
                        geom_text(x = sample_variance_exp, # $\sigma^2$
                                  y = Inf,
                                  vjust = 1,
                                  aes(label = paste("Estimated Variance = ",
                                                    sample_variance_exp)))
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

  # Colour gradient for Population age ranges (M/F) 4e0096, d062f5
  pop_df %>% ggplot(aes(x = grouped_col,
                        fill = after_stat(count))) +
             labs(title = paste0({{ranging_column}},
                                 " Range Plot"),
                  x = paste0({{ranging_column}},
                             " Ranges")) +
             scale_fill_continuous(high = "#4e0096",
                                   low = "#d062f5") +
             geom_bar() +
             geom_text(stat = 'count',
                       aes(label = after_stat(count)), # fix count pls
                       vjust = 0) -> population_age
  
  # Colour gradient for Male age ranges 003b94, 6ac2eb
  pop_df %>% group_by(Sex == "male") %>%
             ggplot(aes(x = grouped_col,
                        fill = after_stat(count))) +
             labs(title = paste0("Male ", {{ranging_column}},
                                 " Range Plot"),
                  x = paste0("Male ", {{ranging_column}},
                             " Ranges")) +
             scale_fill_continuous(high = "#003b94",
                                   low = "#6ac2eb") +
             geom_bar() +
             geom_text(stat = 'count',
                       aes(label = after_stat(count)), # fix count pls
                       vjust = 0) -> male_pop_range
  
  # Colour gradient for Female age ranges 960661, de47a6
  pop_df %>% group_by(Sex == "female") %>%
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
             geom_text(stat = 'count',
                       aes(label = after_stat(count)), # fix count pls
                       vjust = 1) -> female_pop_range
  
  combined_plots <- list(population_age, male_pop_range, female_pop_range)
  
  ggarrange(plotlist = combined_plots,
            ncol = 1,
            nrow = 3)
}
