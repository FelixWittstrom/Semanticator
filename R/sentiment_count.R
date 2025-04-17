#' Count positive and negative sentiment words
#'
#' @param word_vector Vector of words from the document
#' @param positive_words Vector of positive words
#' @param negative_words Vector of negative words
#' @return A list with counts: positive and negative
#' @export
sentiment_count <- function(word_vector, positive_words, negative_words) {
  pos_count <- compare_words(word_vector, positive_words)
  neg_count <- compare_words(word_vector, negative_words)

  return(list(
    positive = pos_count,
    negative = neg_count
  ))
}
