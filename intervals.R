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
  margin <- qt(confidence, df = n - 1)
  lowerinterval <- x_bar - margin
  upperinterval <- x_bar + margin
  print(paste0("Inteveral is: [", lowerinterval, ",", upperinterval, "]"))
  return(c(lowerinterval, upperinterval))
}
