#' Compare word vector to sentiment word list
#'
#' @param word_vector Vector of words from the document
#' @param sentiment_words Vector of words (can include * for wildcard suffix)
#' @return Number of matches found in the text
#' @export
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
