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
  referenece_ngrams <- ngram::get.ngrams(reference_ng)

  result <- (sum(candidate_ngrams %in% reference_ngrams)/length(reference_ngrams))
  return(result)
}

