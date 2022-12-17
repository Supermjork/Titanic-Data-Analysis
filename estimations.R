# This poopoohead takes strings poggers
mme_estimator <- function(population, sample_size, col_name) {
  # Getting the population mean, mu
  true_mean <- mean(population[[col_name]])
  # Gathering a sample
  sample <- sample_n(population, size = sample_size)
  # Calculating the mean of said gathered sample
  estimated_mean_value <- mean(sample[[col_name]])
  estimated_var_value <- var(sample[[col_name]])
  
  bias <- estimated_mean_value - true_mean
  # Gives the bias of the estimator (Not absolute)
  paste0("Estimated Mean: ", estimated_mean_value,
         ", bias: ", bias,
         ", Mean squared Error: ", (estimated_var_value + bias^2))
}

nll <- function(pars, data) {
  # Extract parameters from the vector
  mu <- pars[1]
  sigma <- pars[2]
  # Calculate Negative Log-LIkelihood
  nll <-  -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}

mle_estimator <- function(population, sample_size, col_name) {
  # Will write later (Later is now)
  sample <- sample_n(population, size = sample_size)
  return(optim(par = c(mu = 10, sigma = 12),
               fn = nll, data = sample[[col_name]],
               control = list(parscale = c(mu = 10, sigma = 12))))
}

mean_square_error <- function(sample, size, col_name, pass_population) {
  mle_estimated_values <- mle_estimator(pass_populuation, size, col_name)
  
  mle_estimated_mean <- mle_estimated_values$pars[1]
  mle_estimated_var <- mle_estimated_values$pars[2]
  
  true_mean <- mean(pass_population[[col_name]])
  
  bias <- estimated_mean - true_mean
  mse <- estimated_var + bias^2
}
