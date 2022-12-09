# Answer to Q1, using summarize function
  # Trying to summarise multiple columns
    # Putting labels, functions into vectors and list for encapsulation
    # (Would've been better to just use na.omit(titanic) tbh but it's in Q4)
summarised_columns <- c("Age", "Fare")
summary_functions <- list(min = ~ min(., na.rm = TRUE),
                          max = ~ max(., na.rm = TRUE),
                          q1 = ~ quantile(., 0.25, na.rm = TRUE),
                          q3 = ~ quantile(., 0.75, na.rm = TRUE),
                          mean = ~ mean(., na.rm = TRUE),
                          sigma = ~ sd(., na.rm = TRUE),
                          iqr = ~ IQR(., na.rm = TRUE))

summary_matrix_row <- c("Minimum Value",
                        "Maximum Value",
                        "First Quartile",
                        "Third Quartile",
                        "Mean Population",
                        "Sigma Population",
                        "Inter Quantile Range")

    # Putting summarised data into variable
matrix_summary <- function(sample_data) {

  matrix_summarise <- sample_data %>% summarise_at(summarised_columns,
                                              summary_functions)
  #titanic_summarise
    # Printing summarised data in a neat manner instead of 1 continuous row
  matrix(matrix_summarise, nrow = length(summary_functions),
       ncol = length(summarised_columns),
       dimnames = list(summary_matrix_row, summarised_columns),
       byrow = TRUE)

}
