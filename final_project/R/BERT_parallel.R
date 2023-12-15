library(reticulate)
use_python("/Library/Frameworks/Python.framework/Versions/3.11/bin/python3", required = TRUE)
py_config()
library(text)
library(tidyverse)
library(textTinyR)
library(parallel)
library(future)
library(future.apply)


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

# Modified main function to process all threads in parallel using future and future.apply
process_threads_parallel <- function(df_list, base_name) {
  num_cores <- detectCores() - 1  # Use one less than total to avoid overloading
  plan(multisession, workers = num_cores)
  
  # Split df_list into batches of 5
  batches <- split(df_list, ceiling(seq_along(df_list) / 5))
  
  # Process each batch in parallel
  future_lapply(seq_along(batches), function(i) {
    batch <- batches[[i]]
    all_embeddings <- lapply(batch, function(df_item) {
      df <- df_item$nodes %>% as.data.frame()
      process_comments(df)
    })
    all_embeddings <- all_embeddings[!sapply(all_embeddings, is.null)]
    
    if (length(all_embeddings) > 0) {
      save_embeddings(all_embeddings, generate_filename(base_name, i))
    }
  })
}


# Modified main function to process all threads in parallel with progress tracking
process_threads_parallel <- function(df_list, base_name) {
  num_cores <- detectCores() - 1  # Use one less than total to avoid overloading
  plan(multisession, workers = num_cores)
  
  # Split df_list into batches of 5
  batches <- split(df_list, ceiling(seq_along(df_list) / 5))
  
  # Process each batch in parallel
  future_lapply(seq_along(batches), function(i) {
    print(paste("Processing batch", i, "of", length(batches)))
    batch <- batches[[i]]
    all_embeddings <- lapply(batch, function(df_item, index) {
      print(paste("  - Processing thread", index, "in batch", i))
      df <- df_item$nodes %>% as.data.frame()
      result <- process_comments(df)
      print(paste("  - Finished thread", index, "in batch", i))
      result
    }, index = seq_along(batch))
    all_embeddings <- all_embeddings[!sapply(all_embeddings, is.null)]
    
    if (length(all_embeddings) > 0) {
      save_embeddings(all_embeddings, generate_filename(base_name, i))
      print(paste("Saved embeddings for batch", i))
    }
  })
}

# Load data and process in parallel with progress tracking
df_list <- readRDS(file = "news_top_year_50_comments.rds")
base_name <- gsub("\\.rds$", "", basename("news_top_year_50_comments.rds"))
process_threads_parallel(df_list, base_name)



