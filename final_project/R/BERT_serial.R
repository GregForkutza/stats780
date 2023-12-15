library(reticulate)
use_python("/Library/Frameworks/Python.framework/Versions/3.11/bin/python3", required = TRUE)
py_config()
library(text)
library(tidyverse)
library(textTinyR)

# Modular Version to embed texts

library(dplyr)
library(text)

# Function to process comments and get embeddings
process_comments <- function(df) {
  df_clean <- df %>%
    mutate(clean_body = gsub("[^[:alnum:] ]", "", text)) %>%
    mutate(clean_body = tolower(clean_body)) %>%
    filter(text != "[deleted]", text != "[removed]")
  
  comments <- df_clean$clean_body
  comments <- comments[comments != "" & !is.na(comments)]
  
  if (length(comments) > 0) {
    textEmbed(
      texts = comments,
      model = "bert-base-uncased",
      layers = -2,
      aggregation_from_layers_to_tokens = "mean",
      aggregation_from_tokens_to_texts = "mean",
      keep_token_embeddings = TRUE
    )
  } else {
    NULL
  }
}

# Function to save embeddings
save_embeddings <- function(embeddings, file_name) {
  saveRDS(embeddings, file = file_name)
}

# Function to generate file name
generate_filename <- function(base_name, index) {
  parts <- unlist(strsplit(base_name, "_"))
  paste0("data/embeddings/", paste(parts, collapse = "_"), "_", index, ".rds")
}

# Main function to process all threads
process_threads <- function(df_list, base_name) {
  all_embeddings <- list()
  
  for (i in 1:length(df_list)) {
    df <- df_list[[i]]$nodes %>% as.data.frame()
    embeddings <- process_comments(df)
    
    if (!is.null(embeddings)) {
      all_embeddings[[i]] <- embeddings
    }
    
    if (i %% 5 == 0 || i == length(df_list)) {
      save_embeddings(all_embeddings, generate_filename(base_name, i))
      all_embeddings <- list()
    }
  }
}

# Load data and process
df_list <- readRDS(file = "news_top_year_50_comments.rds")
base_name <- gsub("\\.rds$", "", basename("news_top_year_50_comments.rds"))
process_threads(df_list, base_name)


























# load comments
df_list <- readRDS(file = "news_top_year_50_comments.rds")

# Function to generate file name
generate_filename <- function(base_name, index) {
  parts <- unlist(strsplit(base_name, "_"))
  paste0("data/embeddings/", paste(parts, collapse = "_"), "_", index, ".rds")
}

all_embeddings <- list()
base_name <- gsub("\\.rds$", "", basename("news_top_year_50_comments.rds"))

# Loop over each thread
for (i in 1:length(df_list)) {
  df <- df_list[[i]]$nodes %>% as.data.frame()
  
  # Clean and preprocess the data
  df_clean <- df %>%
    mutate(clean_body = gsub("[^[:alnum:] ]", "", text)) %>%  # Remove special characters
    mutate(clean_body = tolower(clean_body)) %>%
    filter(text != "[deleted]", text != "[removed]")
  
  # Extract text column
  comments <- df_clean$clean_body
  
  # Filter out empty comments
  comments <- comments[comments != "" & !is.na(comments)]
  
  # Check if there are comments to process
  if (length(comments) > 0) {
    # Get embeddings for the thread
    word_embeddings <- textEmbed(
      texts = comments,
      model = "bert-base-uncased",
      layers = -2,  # using the second-to-last layer
      aggregation_from_layers_to_tokens = "mean",
      aggregation_from_tokens_to_texts = "mean",
      keep_token_embeddings = TRUE
    )
    
    # Store the embeddings
    all_embeddings[[i]] <- word_embeddings
  }
  
  # Save and reset every 5 threads
  if (i %% 5 == 0 || i == length(df_list)) {
    saveRDS(all_embeddings, file = generate_filename(base_name, i))
    all_embeddings <- list()  # Reset the list
  }
}
