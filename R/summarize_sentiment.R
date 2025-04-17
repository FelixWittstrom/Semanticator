#' Summarize sentiment with ratio and total word count
#'
#' @param word_vector Vector of words from the document
#' @param positive_words Vector of positive words
#' @param negative_words Vector of negative words
#' @return A list with positive count, negative count, ratio, and total words
#' @export
#'
#' @examples
#' # Example usage:
#' words <- preprocess_text("C:/Users/felwit/Documents/Offline work/Courses/Intermediate R/Group Exam/Example_negative.txt")
#' positive_words <- c("hope", "cooperate", "pass")
#' negative_words <- c("nauseous", "tired", "heavy", "frustrating", "fatigue", "restless", "trapped")
#' counts <- sentiment_count(words, positive_words, negative_words)
#' print(counts)
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
