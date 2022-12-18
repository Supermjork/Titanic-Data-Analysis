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
  print(paste0("Estimated Mean: ", estimated_mean_value,
         ", bias: ", bias,
         ", Mean squared Error: ", (estimated_var_value + bias^2)))
  return(c(estimated_mean_value, estimated_var_value))
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

mean_square_error <- function(mle_values, mme_values, population, col_name) {
  # WHY DOIES THIS NEED TWO SAUQIUARE BRAL<KCKET?!S?!
  mle_estimated_mean <- mle_values$par[[1]]
  mle_estimated_var <- mle_values$par[[2]]

  mme_estimated_mean <- mme_values[1]
  mme_estimated_var <- mme_values[2]

  true_mean <- mean(population[[col_name]])

  mle_bias <- mle_estimated_mean - true_mean
  mle_mse <- mle_estimated_var + mle_bias^2

  mme_bias <- mme_estimated_mean - true_mean
  mme_mse <- mme_estimated_var + mme_bias^2
  print("MME MSE")
  mme_mse
  print("MLE MSE")
  mle_mse
  mse_diff <- mle_mse - mme_mse
  if (mse_diff < 0) {
   print("The MLE is much better )))))))))))")
  }else if (mse_diff > 0) {
   print("The MME is much better )))))))))))")
  } else {
    print("they are the same picture")
  }
  return(c(mme = mme_mse, mle = mle_mse))

}
calc_mse <- function(est_mean, est_var, true_mean) {
  bias <- est_mean - true_mean
  mse <- est_var + (bias)^2
  return(mse)
}
