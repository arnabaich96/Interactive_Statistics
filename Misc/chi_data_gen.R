set.seed(123)  # For reproducibility

# Number of rows and columns
n_rows <- 8
n_cols <- 10

# Simulate marginal totals
row_totals <- sample(50:100, n_rows, replace = TRUE)  # Random row totals
col_totals <- sample(50:100, n_cols, replace = TRUE)  # Random column totals

# Calculate grand total
grand_total <- sum(row_totals)

# Generate a contingency table assuming independence
data_matrix <- matrix(0, nrow = n_rows, ncol = n_cols)
for (i in 1:n_rows) {
  for (j in 1:n_cols) {
    # Expected frequency under independence
    data_matrix[i, j] <- round((row_totals[i] * col_totals[j]) / grand_total)**2
  }
}

# Convert the matrix to a data frame
data_frame <- as.data.frame(data_matrix)

# Add row and column names for clarity
rownames(data_frame) <- paste("Row", 1:n_rows, sep = "")
colnames(data_frame) <- paste("Col", 1:n_cols, sep = "")

# View the generated data
print(data_frame)

# save as chi-dataset.csv
write.csv(data_frame, "chi-dataset.csv", row.names = FALSE)
