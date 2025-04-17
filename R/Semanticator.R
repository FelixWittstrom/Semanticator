
#' Preprocess text from a .txt file
#'
#' @param filepath Path to the text file
#' @return A vector of lowercase, punctuation-free words
#' @export
preprocess_text <- function(filepath) {
  # Read the whole file as one string
  text <- paste(readLines(filepath, warn = FALSE), collapse = " ")

  # Convert to lowercase
  text <- tolower(text)

  # Remove punctuation
  text <- gsub("[[:punct:]]", "", text)

  # Split into words
  word_vector <- unlist(strsplit(text, "\\s+"))

  return(word_vector)
}

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


#' Summarize sentiment with ratio and total word count
#'
#' @param word_vector Vector of words from the document
#' @param positive_words Vector of positive words
#' @param negative_words Vector of negative words
#' @return A list with positive count, negative count, ratio, and total words
#' @export
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


words <- preprocess_text("C:/Users/felwit/Documents/Offline work/Courses/Intermediate R/Group Exam/Example_negative.txt")

# Define word lists
positive_words <- c("hope", "cooperat*", "pass*")
negative_words <- c("nause*", "tired", "heavy", "frustrat*", "fatigue", "restless", "trapped")

counts <- sentiment_count(words, positive_words, negative_words)
print(counts)

summary <- summarize_sentiment(words, positive_words, negative_words)
print(summary)
