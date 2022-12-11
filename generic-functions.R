sample_mean <- function(sample_passed, sample_size, sample_reps, col_name) {
  sample_passed %>% rep_sample_n(size = sample_size,
                                 reps = sample_reps,
                                 replace = TRUE) %>%
    summarise(x_bar = mean({{col_name}}))
  # If you're lazy about it, write Age in mean(), but I want to generic
  # I swear it worked but I broke it and can't remember how it was written
}

# Another fancy function to plot the samples (Has to be used with above fn)
sample_mean_plot <- function(passed_sampled_df) {
  mean_of_mean <- mean(passed_sampled_df$x_bar)
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
sample_variance <- function(sample_passed, sample_size, sample_reps, col_name) {
  sample_passed %>% rep_sample_n(size = sample_size,
                                 reps = sample_reps,
                                 replace = TRUE) %>%
    summarise(S_squared = var({{col_name}}))
  # If you're lazy about it, write Age in mean(), but I want to generic
  # I swear it worked but I broke it and can't remember how it was written
}
sample_var_plot <- function(passed_sampled_df) {
  mean_of_mean <- mean(passed_sampled_df$S_squared)
  passed_sampled_df %>% ggplot(aes(x = S_squared)) +
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
                                  aes(label = paste("Estimated Variance = ",
                                                    mean_of_mean)))
}

# Function to save plots, will take the plot variable as first argument (pipe)
# extension is hardcoded to .pdf
# will be defaulting the name to the variable's name because yes
# takes width and height, you can specify the unit or we could hardcode it
save_plot <- function(plot_to_save, width, height, foldername) {
  ggsave(filename = paste0(plot_to_save$labels$title, ".pdf"), #cant open file?
         plot = plot_to_save,
         path = foldername,
         width = width, height = height, units = "px")
}
