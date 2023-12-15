library(reticulate)
use_virtualenv("/Users/SenseiGregory/.virtualenvs/r-reticulate", required = TRUE)
vader <- import("vaderSentiment.vaderSentiment")
analyzer <- vader$SentimentIntensityAnalyzer()

get_vader_sentiment <- function(text) {
  return(analyzer$polarity_scores(text)$compound)
}

# Apply to your comments
final_df_26_30_pca_kmeans$comment_vader_sentiment <- sapply(final_df_26_30_pca_kmeans$comment, get_vader_sentiment)


apply_vader_to_rds <- function(file_path) {
  # Load the data frame
  df <- readRDS(file_path)
  
  # Check if 'comment' column exists
  if ("comment" %in% names(df)) {
    # Apply VADER sentiment analysis
    df$comment_vader_sentiment <- sapply(df$comment, get_vader_sentiment)
    
    # Construct the output file path
    base_name <- basename(file_path)
    output_file_path <- paste0("data/sentiment_df/", base_name)
    
    # Save the updated data frame
    saveRDS(df, file = output_file_path)
  } else {
    warning(paste("No 'comment' column in", file_path))
  }
}

# Apply the function to all .rds files in the specified directory
file_paths <- list.files(path = "data/clustering_optimized/", full.names = TRUE, pattern = "\\.rds$")
sapply(file_paths, apply_vader_to_rds)
