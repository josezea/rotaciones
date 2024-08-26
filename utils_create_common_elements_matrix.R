#' Create a Matrix of Common Elements Counts
#'
#' This function creates a matrix where each element represents the count of common elements 
#' between corresponding rows of the input matrix.
#'
#' @param matrix_data A matrix. The input matrix for which the common elements matrix will be computed.
#'
#' @return A square matrix where each entry \code{[i, j]} is the number of common elements 
#' between row \code{i} and row \code{j} of the input matrix.
#'
#' @examples
#' # Example usage:
#' mat <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), nrow = 5, ncol = 2)
#' utils_create_common_elements_matrix(mat)
#'
#' @export
utils_create_common_elements_matrix <- function(matrix_data) {
  
  utils_common_elements_between_rows <- function(row1, row2) {
    length(intersect(row1, row2))
  }
  
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
