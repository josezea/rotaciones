#' Test for Rotative Panel 2-2-2 Properties
#'
#' This function performs various tests to verify that the 2-2-2 rotative panel satisfies specific overlap properties.
#'
#' The function checks the following conditions:
#' - Two contiguous periods should have a 50% overlap.
#' - After two periods, there should be a 0% overlap.
#' - After three periods, there should be a 25% overlap.
#' - After four periods, there should be a 50% overlap.
#'
#' @param block Matrix or Data Frame. A block of the rotative panel to be tested.
#'
#' @return A list containing:
#' \describe{
#'   \item{Panel}{The input block of the rotative panel.}
#'   \item{Rows_CommonElements}{A matrix of common elements between rows.}
#'   \item{commonRows}{Vector of common elements between contiguous rows.}
#'   \item{prop_commonRows}{Proportions of common elements for contiguous rows.}
#'   \item{test_commonRows}{Percentage of rows that satisfy the 50% overlap for contiguous periods.}
#'   \item{commonRowsEach2}{Vector of common elements between rows two periods apart.}
#'   \item{prop_commonRowsEach2}{Proportions of common elements for rows two periods apart.}
#'   \item{test_commonRowsEach2}{Percentage of rows that satisfy the 0% overlap after two periods.}
#'   \item{commonRowsEach3}{Vector of common elements between rows three periods apart.}
#'   \item{prop_commonRowsEach3}{Proportions of common elements for rows three periods apart.}
#'   \item{test_commonRowsEach3}{Percentage of rows that satisfy the 25% overlap after three periods.}
#'   \item{commonRowsEach4}{Vector of common elements between rows four periods apart.}
#'   \item{prop_commonRowsEach4}{Proportions of common elements for rows four periods apart.}
#'   \item{test_commonRowsEach4}{Percentage of rows that satisfy the 50% overlap after four periods.}
#'   \item{globalTest}{The average of all individual tests.}
#' }
#'
#' @examples
#' # Example usage:
#' # Assuming `panel_block` is a matrix representing the rotative panel
#' test_result <- test_contigous222(panel_block)
#'
#' @export
#' 
test_contigous222 <- function(block){
  num_Panels <- ncol(block) 
  common_elements_matrix <- utils_create_common_elements_matrix(block)
  
  contiguous <- NA
  for(i in 1:(nrow(common_elements_matrix) - 1)){
    contiguous[i] <- common_elements_matrix[i,i+1] 
    names(contiguous)[i] <- paste0(i, "-", i+1)
  }
  
  prop_contiguous <- contiguous / num_Panels
  indica_contiguous <- 100 * sum(prop_contiguous == 0.5) / length(contiguous)
  
  contiguousEach2 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 2)){
    contiguousEach2[i] <- common_elements_matrix[i,i+2] # typo Each4
    names(contiguousEach2)[i] <- paste0(i, "-", i + 2)
    
  }
  
  prop_contiguousEach2 <- contiguousEach2 / num_Panels
  indica_contiguousEach2 <- 100 * sum(prop_contiguousEach2 == 0) / length(prop_contiguousEach2)
  
  
  contiguousEach3 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 3)){
    contiguousEach3[i] <- common_elements_matrix[i,i+3] # typo Each4
    names(contiguousEach3)[i] <- paste0(i, "-", i + 3)
  }
  
  prop_contiguousEach3 <- contiguousEach3 / num_Panels
  indica_contiguousEach3 <- 100 * sum(prop_contiguousEach3 == 0.25) / length(contiguousEach3)
  
  
  contiguousEach4 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 4)){
    contiguousEach4[i] <- common_elements_matrix[i,i+4] # typo Each4
    names(contiguousEach4)[i] <- paste0(i, "-", i + 4)
  }
  
  prop_contiguousEach4 <- contiguousEach4 / num_Panels
  indica_contiguousEach4 <- 100 * sum(prop_contiguousEach4 == 0.5) / length(contiguousEach4)
  
  
  outcome <- list(block, common_elements_matrix, 
                  contiguous, prop_contiguous, indica_contiguous,
                  contiguousEach2, prop_contiguousEach2, indica_contiguousEach2,
                  contiguousEach3, prop_contiguousEach3, indica_contiguousEach3,
                  contiguousEach4, prop_contiguousEach4, indica_contiguousEach4,
                  (indica_contiguous +  indica_contiguousEach2 + 
                     indica_contiguousEach3 + indica_contiguousEach4) / 4)
  
  
  
  names(outcome) <- c("Panel", "Rows_CommonElements", 
                      "commonRows", "prop_commonRows", "test_commonRows",
                      "commonRowsEach2", "prop_commonRowsEach2", "test_commonRowsEach2",
                      "commonRowsEach3", "prop_commonRowsEach3", "test_commonRowsEach3",
                      "commonRowsEach4", "prop_commonRowsEach4", "test_commonRowsEach4",
                      "globalTest")
  
  outcome  
  
}

