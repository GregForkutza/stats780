library(dplyr)
library(tidytext)
library(topicmodels)
library(tidyverse)

process_lda <- function(data) {
  data$comment_id <- seq_len(nrow(data))
  
  # Clean and prepare the text
  df_clean <- data %>%
    mutate(clean_body = gsub("[^[:alnum:] ]", "", comment)) %>%
    mutate(clean_body = tolower(clean_body))
  
  # Create a tidy text format
  tidy_text <- df_clean %>%
    unnest_tokens(word, clean_body) %>%
    select(comment_id, word)
  
  # Remove stop words
  tidy_text <- tidy_text %>%
    anti_join(tidytext::stop_words, by = "word")
  
  # Create Document-Term Matrix
  dtm <- tidy_text %>%
    count(comment_id, word) %>%
    cast_dtm(document = comment_id, term = word, value = n)
  
  # Run LDA
  num_topics <- 5
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
  
  # Get the top terms for each topic
  topics <- tidy(lda_model, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # Get the dominant topic for each document
  topics_documents <- tidy(lda_model, matrix = "gamma")
  dominant_topic <- topics_documents %>%
    group_by(document) %>%
    top_n(1, gamma) %>%
    ungroup()
  
  # Merge dominant topic back to original data
  dominant_topic$document <- as.integer(dominant_topic$document)
  dominant_topic <- dominant_topic %>%
    select(document, topic)
  
  data <- data %>%
    left_join(dominant_topic, by = c("comment_id" = "document"))
  
  return(list(all_data = data, dominant_topic = dominant_topic, top_terms = top_terms))
}

# Apply the function to each file
file_paths <- list.files(path = "data/clustering_optimized", full.names = TRUE, pattern = "\\.rds$")
results <- lapply(file_paths, function(file) {
  data <- readRDS(file)
  process_lda(data)
})
