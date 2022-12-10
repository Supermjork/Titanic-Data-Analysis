# This poopoohead takes strings poggers
mme_estimator_bias <- function(population, sample_size, col_name) {
  true_value <- mean(population[[col_name]])
  sample_mean <- sample_n(population, size = sample_size)
  estimate_value <- mean(sample_mean[[col_name]])
  return(estimate_value - true_value)
}
