library(tm)
library(wordcloud)
library(Rtsne)
library(tidyverse)
library(umap)
library(mclust)
library(cluster) 

# Define function to combine embedding matrices for each thread 
# row wise for list of more than one embedding.

combine_embeddings <- function(all_embeddings) {   
  
  # Initialize an empty matrix to store all embeddings
  combined_embeddings_matrix <- NULL
  
  # Loop through each thread's embeddings and concatenate them
  for (embeddings in all_embeddings) {
    # Extract the embeddings matrix for the current thread
    embeddings_matrix <- do.call(rbind, embeddings$embedding$texts$texts) %>% t()
    print(dim(embeddings_matrix))
    
    # Combine with the main embeddings matrix
    if (is.null(combined_embeddings_matrix)) {
      combined_embeddings_matrix <- embeddings_matrix
    } else {
      combined_embeddings_matrix <- rbind(combined_embeddings_matrix, embeddings_matrix)
    }
  }
  return(combined_embeddings_matrix)
}


# Define function to concatenate comment thread for list with multiple embeddings

# Define fuction to clean comments
clean_comments <- function(df) {
  df_clean <- df %>%
    mutate(clean_body = gsub("[^[:alnum:] ]", "", text)) %>%
    mutate(clean_body = tolower(clean_body)) %>%
    filter(text != "[deleted]", text != "[removed]") %>%
    select(clean_body, score, children, date)  # Select additional columns
  
  df_clean <- na.omit(df_clean)  # Remove rows with NA values
  print(dim(df_clean))
  return(df_clean)
}

# Define function to loop over df in list
concatenate_comments <- function(dfs, size_lower, size_upper) {
  # Initialize an empty data frame to store all comments and metrics
  all_comments_df <- data.frame()
  
  for (i in size_lower:size_upper) {
    clean_df <- clean_comments(dfs[[i]]$nodes)
    all_comments_df <- rbind(all_comments_df, clean_df)  # Combine data frames
  }
  return(all_comments_df)
}

# Perform Clustering

perform_optimization <- function(embedding_matrix, dim_reduction_method, cluster_method, cluster_range = 2:5) {
  set.seed(123)
  
  # Define specific perplexity values for PCA+t-SNE
  perplexity_range <- c(5, 15, 30)
  
  # Initialize variables to store the best configuration
  best_silhouette_score <- 0
  optimal_n_clusters <- 2
  optimal_perplexity <- 5
  
  # Dimension Reduction and Clustering
  if (dim_reduction_method == "pca") {
    pca_result <- prcomp(embedding_matrix, center = TRUE, scale. = TRUE)
    for (perplexity in perplexity_range) {
      tsne_result <- Rtsne(pca_result$x[, 1:min(ncol(pca_result$x), 50)], dims = 2, perplexity = perplexity, check_duplicates = FALSE)
      results <- tsne_result$Y
      
      # Find optimal number of clusters for this perplexity
      for (n in cluster_range) {
        clustering_result <- perform_clustering(results, cluster_method, n)
        silhouette_score <- calculate_silhouette_score(clustering_result, results)
        
        if (silhouette_score > best_silhouette_score) {
          best_silhouette_score <- silhouette_score
          optimal_n_clusters <- n
          optimal_perplexity <- perplexity
        }
      }
    }
    # Final clustering with best perplexity and number of clusters
    final_tsne_result <- Rtsne(pca_result$x[, 1:min(ncol(pca_result$x), 50)], dims = 2, perplexity = optimal_perplexity, check_duplicates = FALSE)
    final_results <- final_tsne_result$Y
  } else if (dim_reduction_method == "umap") {
    umap_result <- umap(embedding_matrix)
    final_results <- umap_result$layout
    # Optimize only the number of clusters for UMAP
    for (n in cluster_range) {
      clustering_result <- perform_clustering(final_results, cluster_method, n)
      silhouette_score <- calculate_silhouette_score(clustering_result, final_results)
      
      if (silhouette_score > best_silhouette_score) {
        best_silhouette_score <- silhouette_score
        optimal_n_clusters <- n
      }
    }
  } else {
    stop("Invalid value for dimension reduction method")
  }
  
  # Perform final clustering with optimal parameters
  final_clusters <- perform_clustering(final_results, cluster_method, optimal_n_clusters)
  return(list(
    embeddings = final_results, 
    cluster = final_clusters, 
    optimal_n_clusters = optimal_n_clusters, 
    best_silhouette_score = best_silhouette_score,
    optimal_perplexity = ifelse(dim_reduction_method == "pca", optimal_perplexity, NA)
  ))
}

# Helper function for clustering
perform_clustering <- function(data, method, n_clusters) {
  if (method == "kmeans") {
    return(kmeans(data, centers = n_clusters)$cluster)
  } else if (method == "gmm") {
    return(Mclust(data, G = n_clusters)$classification)
  } else {
    stop("Invalid clustering method")
  }
}

# Helper function to calculate silhouette score
calculate_silhouette_score <- function(clustering_result, data) {
  dist_mat <- dist(data)
  return(mean(silhouette(clustering_result, dist_mat)[, "sil_width"]))
}


# Function to combine comments with their corresponding cluster    

combine_cluster_comments <- function (comments, embeddings_matrix, clusters) {
  df_final <- data.frame()
  if (length(comments) == nrow(embeddings_matrix)) {
    df_final <- data.frame(comment = comments, cluster = clusters)  
  } else {
    warning("Mismatch between number of comments and number of rows in embeddings matrix")
  }
  df_final <- df_final[sample(nrow(df_final)),]
  return(df_final)  
}





# Updated process_and_cluster_comments function
process_and_cluster_comments <- function(all_embeddings, dfs, size_lower, size_upper, dim_reduction_method, cluster_method) {
  combined_embeddings_matrix <- combine_embeddings(all_embeddings)
  all_comments_df <- concatenate_comments(dfs, size_lower, size_upper)
  
  # Perform optimization to get the best clustering configuration
  cluster_results <- perform_optimization(combined_embeddings_matrix, dim_reduction_method, cluster_method)
  
  # Combine the comments and metrics with clusters
  final_df <- combine_cluster_comments(all_comments_df$clean_body, combined_embeddings_matrix, cluster_results$cluster)
  
  # Add the metrics and optimal configuration to the final data frame
  final_df <- cbind(final_df, all_comments_df[, c("score", "children", "date")], 
                    optimal_n_clusters = cluster_results$optimal_n_clusters, 
                    best_silhouette_score = cluster_results$best_silhouette_score,
                    optimal_perplexity = cluster_results$optimal_perplexity)
  
  return(final_df)
}


#### Loop over all emebedding files and pairs of dimension reduction and clustering methods. 

# Function to determine lower and upper bounds based on file name
get_bounds <- function(file_name) {
  # Extract the number right before ".rds"
  num <- as.numeric(sub(".*_([0-9]+)\\.rds$", "\\1", basename(file_name)))
  
  lower <- num
  upper <- ifelse(num == 1, 3, ifelse(num == 4, 10, num + 4))
  return(c(lower, upper))
}

# Updated process_and_save_comments function
process_and_save_comments <- function(file_names, dfs, reduction_methods, clustering_methods) {
  for (file_path in file_names) {
    all_embeddings <- readRDS(file_path)
    bounds <- get_bounds(file_path)
    lower <- bounds[1]
    upper <- bounds[2]
    
    best_overall_score <- -Inf
    best_df <- NULL
    best_method_combination <- NULL
    
    for (reduction_method in reduction_methods) {
      for (clustering_method in clustering_methods) {
        result_df <- process_and_cluster_comments(all_embeddings, dfs, lower, upper, reduction_method, clustering_method)
        
        # Check if this combination has the best silhouette score
        current_best_score <- max(result_df$best_silhouette_score, na.rm = TRUE)
        if (current_best_score > best_overall_score) {
          best_overall_score <- current_best_score
          best_df <- result_df
          best_method_combination <- paste(reduction_method, clustering_method, sep = "_")
        }
      }
    }
    
    # Save the best dataframe
    if (!is.null(best_df)) {
      df_name <- paste0("final_df_", lower, "_", upper, "_", best_method_combination)
      saveRDS(best_df, paste0("data/final_df/", df_name, ".rds"))
    }
  }
}



# Function to extract the numeric part from the file name
extract_number <- function(file_name) {
  as.numeric(sub(".*_([0-9]+)\\.rds$", "\\1", basename(file_name)))
}

# Execute the function
file_names <- list.files(path = "data/embeddings", full.names = TRUE, pattern = "\\.rds$")
file_names <- file_names[order(sapply(file_names, extract_number))]
reduction_methods <- c("umap", "pca")
clustering_methods <- c("gmm", "kmeans")
dfs <- readRDS(file = "sorted_news_top_year_50_comments_correct.rds")

process_and_save_comments(file_names, dfs, reduction_methods, clustering_methods)
