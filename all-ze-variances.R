sample_variance <- function(sample_passed, sample_size, sample_reps, col_name) {
  sample_passed %>% rep_sample_n(size = sample_size,
                                 reps = sample_reps,
                                 replace = TRUE) %>%
    summarise(s_squared = var({{col_name}}))
}
sample_var_plot <- function(passed_sampled_df) {
  mean_of_mean <- mean(passed_sampled_df$s_squared)
  passed_sampled_df %>% ggplot(aes(x = s_squared)) +
                        labs(title = deparse(substitute(passed_sampled_df))) +
                        geom_histogram(binwidth = 5,
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
