% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Semanticator.R, R/summarize_sentiment.R
\name{summarize_sentiment}
\alias{summarize_sentiment}
\title{Summarize sentiment with ratio and total word count}
\usage{
summarize_sentiment(word_vector, positive_words, negative_words)

summarize_sentiment(word_vector, positive_words, negative_words)
}
\arguments{
\item{word_vector}{Vector of words from the document}

\item{positive_words}{Vector of positive words}

\item{negative_words}{Vector of negative words}
}
\value{
A list with positive count, negative count, ratio, and total words

A list with positive count, negative count, ratio, and total words
}
\description{
Summarize sentiment with ratio and total word count

Summarize sentiment with ratio and total word count
}
\examples{
# Example usage:
words <- preprocess_text("C:/Users/felwit/Documents/Offline work/Courses/Intermediate R/Group Exam/Example_negative.txt")
positive_words <- c("hope", "cooperate", "pass")
negative_words <- c("nauseous", "tired", "heavy", "frustrating", "fatigue", "restless", "trapped")
counts <- sentiment_count(words, positive_words, negative_words)
print(counts)
summary <- summarize_sentiment(words, positive_words, negative_words)
print(summary)
}
