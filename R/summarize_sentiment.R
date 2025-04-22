#' summarize_sentiment
#'
#' Summarize sentiment with ratio and total word count
#'
#' @param word_vector Vector of words from the document
#' @param positive_words Vector of positive words
#' @param negative_words Vector of negative words
#' @return A list with positive count, negative count, ratio, and total words
#' @examples
#' #
#' summary <- summarize_sentiment(words, positive_words, negative_words)
#' print(summary)
summarize_sentiment <- function(word_vector, positive_words, negative_words) {
  counts <- sentiment_count(word_vector, positive_words, negative_words)
  total_words <- length(word_vector)

  ratio <- if (counts$negative == 0) NA else round(counts$positive / counts$negative, 2)

  return(list(
    total_words = total_words,
    positive = counts$positive,
    negative = counts$negative,
    ratio = ratio
  ))
}
