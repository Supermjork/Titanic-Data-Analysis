# Trying to summarise multiple columns
# Putting labels, functions into vectors and list for encapsulation
# (Would've been better to just use na.omit(titanic) tbh but it's in Q4)

matrix_summary <- function(sample_data, summary_columns,
                           summary_functions, summary_matrix_row) {
  
  # Putting summarised data into variable
  summarised_matrix <- sample_data %>% summarise_at(summary_columns,
                                                    summary_functions)
  
  # Printing summarised data in a neat manner instead of 1 continuous row
  matrix(summarised_matrix,
         nrow = length(summary_functions),
         ncol = length(summary_columns),
         dimnames = list(summary_matrix_row, summary_columns),
         byrow = TRUE)
}
