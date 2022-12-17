# Für du mein freund )))))
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
                        geom_histogram(binwidth = 5,
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
                                  hjust = "inward",
                                  aes(label = paste("Estimated Variance = ",
                                                    signif(sample_variance_exp,
                                                           8))))
}
