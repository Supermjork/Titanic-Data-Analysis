# This poopoohead takes strings poggers
mme_estimator_bias <- function(population, sample_size, col_name) {
  # Getting the population mean, mu
  true_value <- mean(population[[col_name]])
  # Gathering a sample
  sample_mean <- sample_n(population, size = sample_size)
  # Calculating the mean of said gathered sample
  estimate_value <- mean(sample_mean[[col_name]])
  # Gives the bias of the estimator (Not absolute)
  return(paste0("Estimated Mean: ", estimate_value,
         ", bias: ", estimate_value - true_value))
}

nll <- function(pars, data) {
  # Extract parameters from the vector
  mu <- pars[1]
  sigma <- pars[2]
  # Calculate Negative Log-LIkelihood
  nll <-  -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}

mle_estimator_bias <- function(population, sample_size, col_name) {
  # Will write later (Later is now)
  sample <- sample_n(population, size = sample_size)
  return(optim(par = c(mu = 10, sigma = 12),
  fn = nll, data = sample[[col_name]],
  control = list(parscale = c(mu = 10, sigma = 12))))
}
