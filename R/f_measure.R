#' F-Measure
#'
#' For given recall and precision values, computes the F1-score
#' @param recall a single recall value
#' @param precision a single precision value
#' @return The F1-score
#' @export
#'

f_measure <- function(recall, precision){
  result <- 2*((precision*recall)/(precision+recall))
  return(result)
}
