#' Rouge-S: Skip-Bigram Co-occurence Statistics
#'
#' Takes a candidate summary and reference summary, returning the associated ROUGE-S recall, precision, and F values.
#' @param candidate the candidate (usually machine generated) summary
#' @param reference the reference (usually human written) summary
#' @return a dataframe, consisting of a single observation and three variables: recall, precision, and f_measure.
#' @param k the maximum skip distance, default 1
#' @param beta a measure of the importance of precision in relation to recall in the F-measure, default 1—implying equal importance.
#' @import tokenizers
#' @export

rouge_s <- function(candidate, reference, k = 1, beta = 1){
  candidate_skipbo <- tokenizers::tokenize_skip_ngrams(candidate, n_min = 2, k = k, simplify = TRUE)
  reference_skipbo <- tokenizers::tokenize_skip_ngrams(reference, n_min = 2, k = k, simplify = TRUE)

  candidate_length <- length(tokenizers::tokenize_ngrams(candidate, n = 1, simplify = TRUE))
  reference_length <- length(tokenizers::tokenize_ngrams(reference, n = 1, simplify = TRUE))

  overlap <- sum(candidate_skipbo %in% reference_skipbo)

  candidate_combination <- factorial(candidate_length)/(factorial(k)^2)
  reference_combination <- factorial(reference_length)/(factorial(k)^2)

  recall <- overlap/reference_combination
  precision <- overlap/reference_combination
  f_measure <- ((1+beta^2)*recall*precision)/(recall+(beta^2)*precision)

  result <- data.frame(recall, precision, f_measure)
  return(result)
}
