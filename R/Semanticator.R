
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

#' Count matching sentiment words
#'
#' @param word_vector Vector of words from the document
#' @param sentiment_words Vector of positive or negative words
#' @return Number of matches found
#' @export
compare_words <- function(word_vector, sentiment_words) {
  sum(word_vector %in% sentiment_words)
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

positive_words <- c("hope", "cooperate", "pass")
negative_words <- c("nauseous", "tired", "heavy", "frustrating", "fatigue", "restless", "trapped")

counts <- sentiment_count(words, positive_words, negative_words)
print(counts)

summary <- summarize_sentiment(words, positive_words, negative_words)
print(summary)
