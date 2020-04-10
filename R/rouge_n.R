#' Rouge N
#'
#' Takes a candidate summary, reference summary, and n value.  Returns a ROUGE score for the given N.
#' @param candidate the candidate (usually machine generated) summary
#' @param reference the reference (usually human written) summary
#' @param n the number of grams to be considered.  eg. n = 2 considers bigrams, generating a ROUGE-2 score.
#' @return The ROUGE-N score
#' @import ngram
#' @export

rouge_n <- function(candidate, reference, n){

  candidate_ng <- ngram::ngram(candidate, n=n)
  reference_ng <- ngram::ngram(reference, n=n)

  candidate_ngrams <- ngram::get.ngrams(candidate_ng)
  reference_ngrams <- ngram::get.ngrams(reference_ng)

  overlap <- sum(candidate_ngrams %in% reference_ngrams)

  recall <- (overlap/length(reference_ngrams))
  precision <- (overlap/length(candidate_ngrams))
  f_measure <- f_measure(recall, precision)

  result <- data.frame(recall, precision, f_measure)
  return(result)
}

