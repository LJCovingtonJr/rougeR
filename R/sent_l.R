#' Sentence Level LCS
#'
#' Generates a sentence-level ROUGE-L score, used in generating a summary-level ROUGE-L
#' @param candidate the candidate (usually machine generated) summary
#' @param reference the reference (usually human written) summary
#' @param n the number of grams to be considered, default 1.
#' @param beta note whatever beta means here
#' @return a dataframe, consisting of a single observation and three variables: recall, precision, and f_measure.
#' @import tokenizers
#' @import qualV
#' @export

# Questions to note:
# What is beta?
# Why does it produce an F measure with 0 LLCS?
# ROUGE-L is impossible lol

sent_l <- function(reference, candidate, n = 1, beta = 1){
  reference_ngrams <- unlist(tokenizers::tokenize_ngrams(reference, n=n))
  candidate_ngrams <- unlist(tokenizers::tokenize_ngrams(candidate, n=n))

  length_lcs <- qualV::LCS(reference_ngrams, candidate_ngrams)$LLCS

  recall <- length_lcs/length(reference_ngrams)
  precision <- length_lcs/length(candidate_ngrams)
  f_measure <- ((1+beta)*r_lcs*p_lcs)/(r_lcs+(beta^2)*p_lcs)

  result <- data.frame(recall, precision, f_measure)
  return(result)
}
