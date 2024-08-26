#'
#' This function generates a minimal block of the 5-0-0 rotative panel for a specified number of periods.
#'
#' @param n_periods Integer. The number of quarters or months.
#' @param vctr_rep1stLetterCols Integer vector. A vector representing the repetitions of the first letter of each column.
#' @param vctr_rep2ndLetterCols Integer vector. A vector representing the repetitions of the second letter of each column.
#' @param str_blockInitLetter Character. The initial letter of the block.
#'
#' @return A minimal block of the 5-0-0 rotative panel.
#'
#' @examples
#' # Example usage:
#' minimal_block222(40, 1:4, c(1, 3,1, 3), "A")
#'
#' @export

minimal_block222 <- function(n_periods, vctr_rep1stLetterCols, 
                             vctr_rep2ndLetterCols,
                             str_blockInitLetter){
  # Para agregar letras a los valores numéricos
  init_letter <- which(LETTERS == toupper(str_blockInitLetter))
  vctr_letras_iniciales <- LETTERS[init_letter:(init_letter + 3)]
  
  # initialize the matrix
  matrix_out <- matrix(NA, nrow = n_periods, ncol = length(vctr_rep1stLetterCols))
  
  # Initialization of the first row
  matrix_out[1,] <- c(1, 1, 1, 1)
  
  # Matrix initialization with respect to the first row, in this part the initial values are set to the maximum possible
  matrix_out[2:6, which(vctr_rep1stLetterCols == 1)] <- c(1, NA, NA, 1, 1)
  matrix_out[2:6,which(vctr_rep1stLetterCols == 2)] <- c(NA, NA, 1, 1, NA)
  matrix_out[2:6,which(vctr_rep1stLetterCols == 3)] <- c(1, NA, NA, NA, NA)
  matrix_out[2:6,which(vctr_rep1stLetterCols == 4)] <- c(NA, NA, NA, NA, NA)
  
  # Initialization of the second row
  for(j in 1:4){
    if(is.na(matrix_out[2,j])){
      matrix_out[2,j] = 2
    } 
  }
  
  # Initialization of the third row
  for(j in 1:4){
    if(matrix_out[1,j] == matrix_out[2,j] ){
      matrix_out[3,j] = matrix_out[2,j] + 1
    }
    if(matrix_out[1,j] != matrix_out[2,j] ){
      matrix_out[3,j] = matrix_out[2,j] 
    }
  }
  
  # Initialization of the fourth row
  for(j in 1:4){
    if(is.na(matrix_out[4,j]) & matrix_out[2,j] != matrix_out[3,j] ){
      matrix_out[4,j] = matrix_out[3,j] 
    }
    if(is.na(matrix_out[4,j]) & matrix_out[2,j] == matrix_out[3,j] &
       vctr_rep1stLetterCols[j] %in% 1:2){
      matrix_out[4,j] = matrix_out[1,j] 
    }
    if(is.na(matrix_out[4,j]) & matrix_out[2,j] == matrix_out[3,j] &
       vctr_rep1stLetterCols[j] %in% 3:4){
      matrix_out[4,j] = matrix_out[3,j] + 1 
    }
  }
  
  #  Initialization of the fifth row
  for(j in 1:4){
    if(is.na(matrix_out[5,j]) & matrix_out[3,j] != matrix_out[4,j] ){
      matrix_out[5,j] = matrix_out[4,j] 
    }
    if(is.na(matrix_out[5,j]) & matrix_out[3,j] == matrix_out[4,j] &
       vctr_rep1stLetterCols[j] %in% 1:2){
      matrix_out[5,j] = matrix_out[2,j] 
    }
    if(is.na(matrix_out[5,j]) & matrix_out[3,j] == matrix_out[4,j] &
       vctr_rep1stLetterCols[j] %in% 3:4){
      matrix_out[5,j] = matrix_out[4,j] + 1 
    }
  }
  
  # The logic depends on where the 2nd letter of each column starts in rows 6 to 8
  # Initialization of the sixth row
  for(j in 1:4){
    if(is.na(matrix_out[6,j]) & matrix_out[4,j] != matrix_out[5,j] ){
      matrix_out[6,j] = matrix_out[5,j] 
    }
    if(is.na(matrix_out[6,j]) & matrix_out[4,j] == matrix_out[5,j] &
       vctr_rep2ndLetterCols[j] == 1){
      matrix_out[6,j] = matrix_out[3,j] 
    }
    if(is.na(matrix_out[6,j]) & matrix_out[4,j] == matrix_out[5,j] &
       vctr_rep2ndLetterCols[j] == 3){
      matrix_out[6,j] = matrix_out[5,j] + 1 
    }
  }
  
  # Initialization of the seventh row
  for(j in 1:4){
    if(is.na(matrix_out[7,j]) & matrix_out[5,j] != matrix_out[6,j] ){
      matrix_out[7,j] = matrix_out[6,j] 
    }
    if(is.na(matrix_out[7,j]) & matrix_out[5,j] == matrix_out[6,j] &
       vctr_rep2ndLetterCols[j] == 1){
      matrix_out[7,j] = matrix_out[4,j] 
    }
    if(is.na(matrix_out[7,j]) & matrix_out[5,j] == matrix_out[6,j] &
       vctr_rep2ndLetterCols[j] == 3){
      matrix_out[7,j] = matrix_out[6,j] + 1 
    }
  }
  # Initialization from the eighth row onwards
  for(i in 8:n_periods){
    for(j in 1:4){
      if(is.na(matrix_out[i,j]) & matrix_out[i-2,j] != matrix_out[i-1,j] ){
        matrix_out[i,j] = matrix_out[i-1,j] 
      }
      if(is.na(matrix_out[i,j]) & matrix_out[i-2,j] == matrix_out[i-1,j] &
         matrix_out[i-3,j] == matrix_out[i-7,j]){
        matrix_out[i,j] = matrix_out[i-1,j] + 1 
      }
      if(is.na(matrix_out[i,j]) & matrix_out[i-2,j] == matrix_out[i-1,j] &
         matrix_out[i-3,j] != matrix_out[i-7,j]){
        matrix_out[i,j] = matrix_out[i-3,j]  
      }
    }
  }
  
  matrix_out2 <- matrix_out
  for(i in 1:4) { # Bloque de 4
    matrix_out2[,i] <- paste0(vctr_letras_iniciales[i], matrix_out[,i]) 
  }
  
  matrix_out2
}