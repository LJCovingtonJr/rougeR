#' Rouge N with multiple reference summaries
#'
#' Takes a candidate summary, more than one reference summary, and a single n value.  Returns a ROUGE score for the given N.
#' @param candidate the candidate (usually machine generated) summary
#' @param references a character vector of reference (usually human written) summaries
#' @param n the number of grams to be considered.  eg. n = 2 considers bigrams, generating a ROUGE-2 score.
#' @return The ROUGE-N score
#' @import ngram
#' @export

multi_rouge_n <- function(candidate, references, n){

  candidate_ng <- ngram::ngram(candidate, n=n)
  reference_ng <- sapply(references, ngram, n=n)

  candidate_ngrams <- ngram::get.ngrams(candidate_ng)
  reference_ngrams <- sapply(reference_ng, get.ngrams)

  # there has to be an easier way to do this, but it seems to work
  overlaps <- vector(mode = "numeric", length = length(reference_ngrams))

  num <- 1
  for (i in reference_ngrams){
    overlaps[num] <- sum(candidate_ngrams %in% reference_ngrams[[num]])
    num <- num+1
  }

  recalls <- vector(mode = "numeric", length = length(reference_ngrams))

  num <- 1
  for (i in overlaps){
    recalls[num] <- overlaps[num]/length(reference_ngrams[[num]])
    num <- num+1
  }

  recall <- max(recalls)
  precision <- max(overlaps/length(candidate_ngrams))
  f_measure <- f_measure(recall, precision)

  result <- data.frame(recall, precision, f_measure)
  return(result)
}
