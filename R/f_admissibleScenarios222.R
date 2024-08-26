#' Compute Plausible Scenarios for the 2-2-2 Rotative Panel
#'
#' This function computes plausible scenarios for the second letter columns (`vctr_rep2ndLetterCols`)
#' in a 2-2-2 rotative panel based on the given number of periods and repetitions of the first letter columns.
#'
#' @param n_periods Integer. Number of quarters or months. Default is 40.
#' @param vctr_rep1stLetterCols Integer vector. A vector of repetitions of the first letter of every column. Default is `1:4`.
#' @param str_blockInitLetter Character. Initial letter of the block (a string).
#'
#' @return A data frame containing the plausible scenarios where the global test passes with 100% success.
#'
#' @examples
#' # Example usage:
#' # Assuming you want to compute plausible scenarios for 40 periods
#' scenarios <- f_admissibleScenarios222(n_periods = 40, vctr_rep1stLetterCols = 1:4, str_blockInitLetter = "A")
#'
#' @export
#' 
f_admissibleScenarios222 <- function(n_periods = 40, vctr_rep1stLetterCols = 1:4, 
                                     str_blockInitLetter){
  grid_2ndLetter <- expand.grid(c(1,3), c(1,3), c(1,3), c(1,3))
  lst_possibleOutcomes <- vector(mode = "list", nrow(grid_2ndLetter))
  for(i in 1:nrow(grid_2ndLetter)){
    lst_possibleOutcomes[[i]] <- minimal_block222(n_periods = n_periods,
                                                  vctr_rep1stLetterCols = vctr_rep1stLetterCols,
                                                  vctr_rep2ndLetterCols =  grid_2ndLetter[i,],
                                                  str_blockInitLetter)
    
  }
  
  vctr_globalTest <- rep(NA_real_, nrow(grid_2ndLetter))
  for(i in 1:nrow(grid_2ndLetter)){
    vctr_globalTest[i] <- test_contigous222(lst_possibleOutcomes[[i]])$globalTest
  }
  df_possibleOutcome <- grid_2ndLetter[which(vctr_globalTest == 100),]
  df_possibleOutcome
}