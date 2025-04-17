#' Count matching sentiment words
#'
#' @param word_vector Vector of words from the document
#' @param sentiment_words Vector of positive or negative words
#' @return Number of matches found
#' @export
compare_words <- function(word_vector, sentiment_words) {
  sum(word_vector %in% sentiment_words)
}
