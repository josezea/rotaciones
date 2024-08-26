# Function to find the number of common elements between two rows
utils_common_elements_between_rows <- function(row1, row2) {
  length(intersect(row1, row2))
}

# Function to create a matrix of common elements counts
utils_create_common_elements_matrix <- function(matrix_data) {
  matrix_data <- as.matrix(matrix_data)
  n <- nrow(matrix_data)
  common_elements_matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      common_elements_matrix[i, j] <- utils_common_elements_between_rows(matrix_data[i, ], 
                                                                         matrix_data[j, ])
    }
  }
  return(common_elements_matrix)
}
