#' preprocess_text
#'
#' This function preprocesses text for the Semanticator package, using a .txt file as input
#'
#' @param filepath Absolute path to the text file
#' @return A vector of lowercase, punctuation-free words
#' @examples
#' # words <- preprocess_text("filepath.../Example_negative.txt")
#' head(words)
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
