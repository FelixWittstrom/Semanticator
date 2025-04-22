#' compare_words
#'
#' This function compares a word vector to sentiment word list
#' The word vector can be created from a .txt document using the preprocess_text function
#' The sentiment word list has to be specified
#'
#' @param word_vector Vector of words from the document
#' @param sentiment_words Vector of words (can include * for suffix or prefix)
#' @return Number of matches found in the text
#' @examples
#' # Define word lists
#' positive_words <- c("hope", "cooperat*", "pass*")
#' negative_words <- c("nause*", "tired", "heavy", "frustrat*", "fatigue", "restless", "trapped")
#'
compare_words <- function(word_vector, sentiment_words) {
  match_count <- 0

  for (pattern in sentiment_words) {
    # Handle suffix wildcard
    if (endsWith(pattern, "*")) {
      prefix <- sub("\\*$", "", pattern)
      matches <- word_vector[startsWith(word_vector, prefix)]
    } else if (startsWith(pattern, "*")) {
      suffix <- sub("^\\*", "", pattern)
      matches <- word_vector[endsWith(word_vector, suffix)]
    } else {
      matches <- word_vector[word_vector == pattern]
    }

    match_count <- match_count + length(matches)
  }

  return(match_count)
}
