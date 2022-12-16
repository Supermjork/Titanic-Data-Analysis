# Returns confidence interval for given sample means and variance
# With confidence and sample size
conf_interval <- function(x_bar, s, n, confidence) {
  # in case people REALLY don't like putting that .
  if (confidence > 1) {
    confidence <- confidence / 100
  }else if (confidence <= 0) {
    print("You are a horrible person >:(")
    return(NULL)
  }
  # Get margin of error
  margin <- qt(confidence, df = n - 1) * s / sqrt(n)
  lowerinterval <- x_bar - margin
  upperinterval <- x_bar + margin
  print(paste0("Interval is: [", lowerinterval, ", ", upperinterval, "]"))
  return(c(lowerinterval, upperinterval))
}
# Same as conf_interval but takes a vector for x_bar and s
conf_interval2 <- function(vec, n, confidence = 0.95) {
  # in case people REALLY don't like putting that .
  if (confidence > 1) {
    confidence <- confidence / 100
  }else if (confidence <= 0) {
    print("You are a horrible person >:(")
    return(NULL)
  }
  x_bar <- vec[1]
  s <- vec[2]
  # Get margin of error
  margin <- qt(confidence, df = n - 1) * s / sqrt(n)
  lowerinterval <- x_bar - margin
  upperinterval <- x_bar + margin
  print(paste0("Interval is: [", lowerinterval, ", ", upperinterval, "]"))
  return(c(lowerinterval, upperinterval))
}
